package net.sf.JRecord.zTest.schema;

import java.io.IOException;
import java.io.StringReader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.cgen.impl.ArrayFieldDefinition;
import junit.framework.TestCase;

public class TstArrayFieldDefinition extends TestCase {

	private static String COPYBOOK_1_DIM
			 = "        01 Test-1-Dim.\n"
			 + "           03 fld1    pic x(3).\n"
			 + "           03         occurs 3.\n"
			 + "              05 fld-2        pic 999.\n"
			 + "              05 fld-3        pic XX.\n";
	
	private static String COPYBOOK_2_DIM
			= COPYBOOK_1_DIM
			+ "             05         occurs 4.\n"
			+ "                07 fld-4        pic 9999.\n"
			+ "                07 fld-5        pic XX.\n";
	
	private static String COPYBOOK_3_DIM
			= COPYBOOK_2_DIM
			+ "                07         occurs 2.\n"
			+ "                   09 fld-6        pic 99999.\n"
			+ "                   09 fld-7        pic XXX.\n";


	public void test01dim() throws IOException  {
		LayoutDetail schema = toLayout(COPYBOOK_1_DIM);

		chkField1dim(schema, "fld-2", 5, 4, 3);
		chkField1dim(schema, "fld-3", 5, 7, 2);
		
		schema = toLayout(COPYBOOK_2_DIM);

		chkField1dim(schema, "fld-2", 29, 4, 3);
		chkField1dim(schema, "fld-3", 29, 7, 2);
		
		schema = toLayout(COPYBOOK_3_DIM);

		chkField1dim(schema, "fld-2", 93, 4, 3);
		chkField1dim(schema, "fld-3", 93, 7, 2);
	}
	

	public void test02dim() throws IOException  {
		LayoutDetail schema = toLayout(COPYBOOK_2_DIM);

		chkField2dim(schema, "fld-4", 6, 9, 4);
		chkField2dim(schema, "fld-5", 6, 13, 2);
		
		schema = toLayout(COPYBOOK_3_DIM);

		chkField2dim(schema, "fld-4", 22, 9, 4);
		chkField2dim(schema, "fld-5", 22, 13, 2);
	}


	public void test03dim() throws IOException  {
		LayoutDetail schema  = toLayout(COPYBOOK_3_DIM);

		chkField3dim(schema, "fld-6", 8, 15, 5);
		chkField3dim(schema, "fld-7", 8, 20, 3);
	}

	/**
	 * @param schema
	 * @param fldName
	 * @param arrayElementSize
	 * @param pos
	 * @param len
	 */
	public void chkField1dim(LayoutDetail schema, String fldName,
			int arrayElementSize, int pos, int len) {
		int arrayCount = 3;
		ArrayFieldDefinition ad = new ArrayFieldDefinition(schema.getRecord(0), arrayCount, 
				schema.getFieldFromName(fldName + " (1)"), 
				schema.getFieldFromName(fldName + " (0)"));
		assertEquals(1, ad.getIndexCount());
		assertEquals(arrayCount, ad.getArrayLength(0));
		assertEquals(arrayElementSize, ad.getArrayElementSize(0));
		for (int i = 0; i < arrayCount; i++) {
			String idx = Integer.toString(i);
			checkFld(fldName, idx, pos + i * arrayElementSize, len, ad.get(i));			
			checkFld(fldName, idx, pos + i * arrayElementSize, len, ad.getField(i));			
		}
	}

	public void chkField2dim(LayoutDetail schema, String fldName,
			int arrayElementSize, int pos, int len) {
		int arrayCount1 = 3;
		int arrayCount2 = 4;
		int size1stIdx = 5 + arrayElementSize * arrayCount2;
		ArrayFieldDefinition ad = new ArrayFieldDefinition(schema.getRecord(0), arrayCount1, 
				schema.getFieldFromName(fldName + " (1, 0)"), 
				schema.getFieldFromName(fldName + " (0, 1)"), 
				schema.getFieldFromName(fldName + " (0, 0)"));
		assertEquals(2, ad.getIndexCount());
		assertEquals(arrayCount1, ad.getArrayLength(0));
		assertEquals(size1stIdx, ad.getArrayElementSize(0));
		assertEquals(arrayCount2, ad.getArrayLength(1));
		assertEquals(arrayElementSize, ad.getArrayElementSize(1));
		for (int j = 0; j < arrayCount1; j++) {
			for (int i = 0; i < arrayCount2; i++) {
				String idx = j + ", " +  i;
				checkFld(fldName, idx, pos + size1stIdx * j + i * arrayElementSize, len, ad.get(j, i));
				checkFld(fldName, idx, pos + size1stIdx * j + i * arrayElementSize, len, ad.getField(j, i));
			}
		}
	}


	public void chkField3dim(LayoutDetail schema, String fldName,
			int arrayElementSize, int pos, int len) {
		int arrayCount1 = 3;
		int arrayCount2 = 4;
		int arrayCount3 = 2;
		int size2stIdx = 6 + arrayElementSize * arrayCount3;
		int size1stIdx = 5 + size2stIdx * arrayCount2;
		ArrayFieldDefinition ad = new ArrayFieldDefinition(schema.getRecord(0), arrayCount1, 
				schema.getFieldFromName(fldName + " (1, 0, 0)"), 
				schema.getFieldFromName(fldName + " (0, 1, 0)"), 
				schema.getFieldFromName(fldName + " (0, 0, 1)"), 
				schema.getFieldFromName(fldName + " (0, 0, 0)"));
		assertEquals(3, ad.getIndexCount());
		assertEquals(arrayCount1, ad.getArrayLength(0));
		assertEquals(size1stIdx, ad.getArrayElementSize(0));
		assertEquals(arrayCount2, ad.getArrayLength(1));
		assertEquals(size2stIdx, ad.getArrayElementSize(1));
		assertEquals(arrayCount3, ad.getArrayLength(2));
		assertEquals(arrayElementSize, ad.getArrayElementSize(2));

		for (int k = 0; k < arrayCount1; k++) {
			for (int j = 0; j < arrayCount2; j++) {
				for (int i = 0; i < arrayCount3; i++) {
					String idx = k + ", " + j + ", " +  i;
					int pos2 = pos + size1stIdx * k + size2stIdx * j + i * arrayElementSize;
					checkFld(fldName, idx, pos2, len, ad.get(k, j, i));
					checkFld(fldName, idx, pos2, len, ad.getField(k, j, i));
				}
			}
		}
	}


	private void checkFld(String fldName, String idx, int pos, int len, IFieldDetail fld) {
		assertEquals(fldName + " (" + idx + ")", fld.getName());
		assertEquals("Idx: " + idx, pos, fld.getPos());			
		assertEquals("Idx: " + idx, len, fld.getLen());
	}

	private LayoutDetail toLayout(String copybook) throws IOException {
		return JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "Copybook")
				.getLayout();
	}
}
