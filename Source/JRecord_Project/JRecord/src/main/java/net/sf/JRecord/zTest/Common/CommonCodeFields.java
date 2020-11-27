package net.sf.JRecord.zTest.Common;

import java.io.IOException;
import java.util.Arrays;

import junit.framework.TestCase;
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
		
		TestCase.assertEquals(opts.fileOrganization, l.getFileStructure());
		TestCase.assertEquals(font, l.getFontName());
	
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
				TestCase.assertEquals(idx, eField.getName(), field.getName());
				TestCase.assertEquals(idx, eField.getPos(),  field.getPos());
				TestCase.assertEquals(idx, eField.getLen(),  field.getLen());
				TestCase.assertEquals(idx, 
						typeMgr.getShortType(eField.getType(), eField.getLen(), charsetType), 
						field.getType());
				TestCase.assertEquals(idx, eField.getDecimal(), field.getDecimal());
				TestCase.assertEquals(idx, font, field.getFontName());
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
				TestCase.assertEquals(id, eField.getName(), field.getName());
				TestCase.assertEquals(id, eField.getPos(),  field.getPos());
				//TestCase.assertEquals(id, eField.getLen(),  field.getLen());
				TestCase.assertEquals(id, eField.getDecimal(), field.getDecimal());
				TestCase.assertEquals(id, eField.getType(), field.getType());
				TestCase.assertEquals(id, font, field.getFontName());
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
