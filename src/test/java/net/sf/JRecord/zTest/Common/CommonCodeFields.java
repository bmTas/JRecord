package net.sf.JRecord.zTest.Common;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeManager.CharsetType;
import net.sf.JRecord.cgen.support.Code2JRecordConstants;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CommonCodeFields {

    private static final String[] JAVA_TYPE_NAME = new String[Type.LAST_SYSTEM_TYPE];

	static {
	   	Arrays.fill(JAVA_TYPE_NAME, null);
	}
  
	public static String getJRecordTypeName(int type) {
		return Code2JRecordConstants.getJRecordTypeName(type);
	}


	public static void check(ICobolIOBuilder bldr, CblBldrOptions opts, FieldDetail[] expected, String id) throws IOException {
		String font = opts.font;
		bldr.setDialect(opts.dialect)
		    .setDropCopybookNameFromFields(opts.dropCopybookNameFromFields)
		    .setFileOrganization(opts.fileOrganization)
		    .setFont(font);
		LayoutDetail l = bldr.getLayout();
		RecordDetail record = l.getRecord(0);
		
		assertEquals(opts.fileOrganization, l.getFileStructure());
		assertEquals(font, l.getFontName());
	
		checkFields(id, font, expected, record);
	}

	public static void checkFields(String id, String font, FieldDetail[] expected, RecordDetail record) {
		if (expected == null) System.out.println("\t}, {");
		TypeManager typeMgr = TypeManager.getInstance();
		CharsetType charsetType = typeMgr.getCharsetType(record.getFontName());
		for (int i = 0; i < record.getFieldCount(); i++) {
			FieldDetail field = record.getField(i);
			if (expected == null) {
				System.out.println(
						  "\t\tCommonCodeFields.createField(\"" + field.getName()
						+ "\", " + field.getPos()
						+ ", "   + field.getLen()
						+ ", "   + field.getDecimal()
						+ ", "   + CommonCodeFields.getJRecordTypeName(field.getType())
						+ "),"
				);
			} else {
				FieldDetail eField = expected[i];
				String idx = id + " " + i + ": " + eField.getName();
				assertEquals( eField.getName(), field.getName(), idx);
				assertEquals( eField.getPos(),  field.getPos(), idx);
				assertEquals( eField.getLen(),  field.getLen(), idx);
				assertEquals( 
						typeMgr.getShortType(eField.getType(), eField.getLen(), charsetType), 
						field.getType(), idx);
				assertEquals( eField.getDecimal(), field.getDecimal(), idx);
				assertEquals( font, field.getFontName(), idx);
			}
		}
	}
	

	public static void checkCsvFields(String id, String font, FieldDetail[] expected, RecordDetail record) {
		if (expected == null) System.out.println("\t}, {");
		for (int i = 0; i < record.getFieldCount(); i++) {
			FieldDetail field = record.getField(i);
			if (expected == null) {
				System.out.println(
						  "\t\tCommonCodeFields.createCsvField(\"" + field.getName()
						+ "\", " + field.getPos()
						+ ", "   + field.getDecimal()
						+ ", "   + CommonCodeFields.getJRecordTypeName(field.getType())
						+ "),"
				);
			} else {
				FieldDetail eField = expected[i];
				assertEquals( eField.getName(), field.getName(), id);
				assertEquals( eField.getPos(),  field.getPos(), id);
				//assertEquals( eField.getLen(),  field.getLen(), id);
				assertEquals( eField.getDecimal(), field.getDecimal(), id);
				assertEquals( eField.getType(), field.getType(), id);
				assertEquals( font, field.getFontName(), id);
			}
		}
	}


	
	public static FieldDetail createField(String name, int pos, int len, int decimal, int type) {
		return FieldDetail.newFixedWidthField(name, type, pos, len, decimal, "");
	}

	
	public static FieldDetail createCsvField(String name, int pos, int decimal, int type) {
		return FieldDetail.newCsvField(name, type, pos, decimal, "");
	}


	public static class CblBldrOptions {
		public final int dialect, fileOrganization;
		public final boolean dropCopybookNameFromFields;
		public final String font;
		
		
		public CblBldrOptions(int dialect, int fileOrganization,
				boolean dropCopybookNameFromFields, String font) {
			super();
			this.dialect = dialect;
			this.fileOrganization = fileOrganization;
			this.dropCopybookNameFromFields = dropCopybookNameFromFields;
			this.font = font;
		}
		
		
	}
}
