package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.IOException;
import java.io.StringReader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.CommonCodeFields;
import junit.framework.TestCase;


/**
 * Testing Keeping & Dropping fillers
 * @author Bruce Martin
 *
 */
public class TstCobolIOBuilderKeepFiller extends TestCase {

	public static final String COPYBOOK_WITH_FILLERS_1
			= "       01  Tst-Record.\n"
			+ "           05 Fld-1             Pic X.\n"
			+ "           05 Filler            Pic X(4).\n"
			+ "           05 Fld-2             Pic X.\n"
			+ "           05 Filler            Pic X(3).\n"
			+ "           05 Fld-3             Pic X.\n"
			+ "           05 Filler            Pic X(2). \n"
			+ "           05 Fld-4             Pic X.\n"
			+ "           05 Filler            Pic X(1).\n"
			+ "           05 Fld-5             Pic X.\n";
	

	public static final String COPYBOOK_WITH_FILLERS_2
			= "       01  Tst-Record.\n"
			+ "           05 Fld-1             Pic X.\n"
			+ "           05                   Pic X(4).\n"
			+ "           05 Fld-2             Pic X.\n"
			+ "           05                   Pic X(3).\n"
			+ "           05 Fld-3             Pic X.\n"
			+ "           05                   Pic X(2). \n"
			+ "           05 Fld-4             Pic X.\n"
			+ "           05                   Pic X(1).\n"
			+ "           05 Fld-5             Pic X.\n";
	
	private FieldDetail EXTRA_FIELD_1 = createField("Filler", 16, 3, Type.ftChar);
	private FieldDetail EXTRA_FIELD_2 = createField("", 16, 3, Type.ftChar);
	
	public static final String EXTRA_FILLER_1 = "           05 Filler            Pic X(3).\n";
	public static final String EXTRA_FILLER_2 = "           05                   Pic X(3).\n";
	
	private static FieldDetail[] EXPECTED_1 = {
		createField("Fld-1", 1, 1, Type.ftChar),
		createField("Filler", 2, 4, Type.ftChar),
		createField("Fld-2", 6, 1, Type.ftChar),
		createField("Filler", 7, 3, Type.ftChar),
		createField("Fld-3", 10, 1, Type.ftChar),
		createField("Filler", 11, 2, Type.ftChar),
		createField("Fld-4", 13, 1, Type.ftChar),
		createField("Filler", 14, 1, Type.ftChar),
		createField("Fld-5", 15, 1, Type.ftChar),
	};
	
	private static FieldDetail[] EXPECTED_2 = {
		createField("Fld-1", 1, 1, Type.ftChar),
		createField("", 2, 4, Type.ftChar),
		createField("Fld-2", 6, 1, Type.ftChar),
		createField("", 7, 3, Type.ftChar),
		createField("Fld-3", 10, 1, Type.ftChar),
		createField("", 11, 2, Type.ftChar),
		createField("Fld-4", 13, 1, Type.ftChar),
		createField("", 14, 1, Type.ftChar),
		createField("Fld-5", 15, 1, Type.ftChar),
	};

	public void testKeepFiller01() throws IOException {
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_1), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(true);
		
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, EXPECTED_1, "keep_1");
	}
	
	
	
	public void testDropFiller01() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length / 2 + 1];

		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_1), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB );
		
		for (int i = 0; i < expected.length; i++) {
			expected[i] = EXPECTED_1[i*2];
		}
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}
	public void testDropFiller02() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length / 2 + 1];
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_1), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(false);
		
		for (int i = 0; i < expected.length; i++) {
			expected[i] = EXPECTED_1[i*2];
		}
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}
	
	
	public void testKeepFiller03() throws IOException {
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_2), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(true);
		
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, EXPECTED_2, "01");
	}
	
	public void testDropFiller03() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length / 2 + 1];

		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_2), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB );
		
		for (int i = 0; i < expected.length; i++) {
			expected[i] = EXPECTED_2[i*2];
		}
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}
	public void testDropFiller04() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length / 2 + 1];
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_2), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(false);
		
		for (int i = 0; i < expected.length; i++) {
			expected[i] = EXPECTED_2[i*2];
		}
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}
	
	

	public void testKeepFiller11() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length + 1];
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_1 + EXTRA_FILLER_1), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(true);
		
		System.arraycopy(EXPECTED_1, 0, expected, 0, EXPECTED_1.length);
		expected[EXPECTED_1.length] = EXTRA_FIELD_1;
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "keep_1");
	}

	public void testDropFiller12() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_1.length / 2 + 2];

		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_1 + EXTRA_FILLER_1), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB );
		
		for (int i = 0; i < expected.length-1; i++) {
			expected[i] = EXPECTED_1[i*2];
		}
		expected[expected.length-1] = EXTRA_FIELD_1;
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}


	public void testKeepFiller14() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_2.length + 1];
		@SuppressWarnings("deprecation")
		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_2 + EXTRA_FILLER_2), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB )
							.setKeepFillers(true);
		
		System.arraycopy(EXPECTED_2, 0, expected, 0, EXPECTED_1.length);
		expected[EXPECTED_2.length] = EXTRA_FIELD_2;
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "keep_1");
	}

	public void testDropFiller14() throws IOException {
		FieldDetail[] expected = new FieldDetail[EXPECTED_2.length / 2 + 2];

		ICobolIOBuilder bldr = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK_WITH_FILLERS_2  + EXTRA_FILLER_2), "Tst-Record")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
							.setFileOrganization(Constants.IO_VB );
		
		for (int i = 0; i < expected.length-1; i++) {
			expected[i] = EXPECTED_2[i*2];
		}
		expected[expected.length-1] = EXTRA_FIELD_2;
		CommonCodeFields.CblBldrOptions opts = new CommonCodeFields.CblBldrOptions(
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_VB , false, "");
		CommonCodeFields.check(bldr, opts, expected, "Drop_1");
	}

	private static FieldDetail createField(String name, int pos, int len, int type) {
		return FieldDetail.newFixedWidthField(name, type, pos, len, 0, "");
	}
}
