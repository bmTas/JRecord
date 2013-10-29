package net.sf.JRecord.zTest.Details;

import java.util.List;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

public class TstGroupFieldAccess1 extends TestCase {
	private String cobolCopybook
			= "      01 COMPANY-RECORD.\n"
			+ "         05 COMPANY-NAME     PIC X(30).\n"
			+ "         05 EMPLOYEE-LIST.\n"
			+ "            10 PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 VICE-PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 OTHERS.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n";

	private int recordIdx = 0;
	private LayoutDetail schema;
	{
		try {
			schema = TestCommonCode.getLayoutFromCobolStr(
							cobolCopybook, "COMPANY-RECORD",
							CopybookLoader.SPLIT_NONE, "", Convert.FMT_INTEL);
		} catch (Exception e) {
			e.printStackTrace();
		}
	};

	public void testFirstName() {
		int[][] fieldDetails = {
				{46, 8, 0},
				{69, 8, 0},
				{102, 8, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<FieldDetail> flds1 = record.getFields("FIRST-NAME");
		List<FieldDetail> flds2 = record.getFields("FIRST-NAME", "EMPLOYEE-LIST");
		List<FieldDetail> flds3 = record.getFields("FIRST-NAME", "EMPLOYEE-LIST", "OTHERS");

		FieldDetail presidentFirstNameFld = record.getUniqueField("FIRST-NAME", "PRESIDENT");
		FieldDetail vicePresidentFirstNameFld = record.getUniqueField("FIRST-NAME", "VICE-PRESIDENT");
		FieldDetail otherFirstNameFld = record.getUniqueField("FIRST-NAME", "OTHERS");

		assertEquals("Fields Retrieved 1", 3, flds1.size());
		assertEquals("Fields Retrieved 2", 3, flds2.size());
		assertEquals("Fields Retrieved 3", 1, flds3.size());

		chkFields("Fields 1: ", flds1, presidentFirstNameFld, vicePresidentFirstNameFld, otherFirstNameFld);
		chkFields("Fields 2: ", flds2, presidentFirstNameFld, vicePresidentFirstNameFld, otherFirstNameFld);

		assertTrue(flds3.get(0) == otherFirstNameFld);

		for (int i = 0; i < fieldDetails.length; i++) {
			IFieldDetail fieldDetail = flds1.get(i);

			assertEquals("check Field Name", "FIRST-NAME", fieldDetail.getName());
			assertEquals("check Field pos", fieldDetails[i][0], fieldDetail.getPos());
			assertEquals("check Field Length", fieldDetails[i][1], fieldDetail.getLen());
			assertEquals("check Field Type", fieldDetails[i][2], fieldDetail.getType());
		}

		try {
			record.getUniqueField("FIRST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields; should be only one", e.getMessage());
		}

		try {
			record.getUniqueField("FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found", e.getMessage());
		}
	}


	public void testLastName() {
		int[][] fieldDetails = {
				{31, 15, 0},
				{54, 15, 0},
				{87, 15, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<FieldDetail> flds1 = record.getFields("LAST-NAME");
		List<FieldDetail> flds2 = record.getFields("LAST-NAME", "EMPLOYEE-LIST");
		List<FieldDetail> flds3 = record.getFields("LAST-NAME", "EMPLOYEE-LIST", "OTHERS");

		FieldDetail presidentFirstNameFld = record.getUniqueField("LAST-NAME", "PRESIDENT");
		FieldDetail vicePresidentFirstNameFld = record.getUniqueField("LAST-NAME", "VICE-PRESIDENT");
		FieldDetail otherFirstNameFld = record.getUniqueField("LAST-NAME", "OTHERS");

		assertEquals("Fields Retrieved 1", 3, flds1.size());
		assertEquals("Fields Retrieved 2", 3, flds2.size());
		assertEquals("Fields Retrieved 3", 1, flds3.size());

		chkFields("Fields 1: ", flds1, presidentFirstNameFld, vicePresidentFirstNameFld, otherFirstNameFld);
		chkFields("Fields 2: ", flds2, presidentFirstNameFld, vicePresidentFirstNameFld, otherFirstNameFld);

		assertTrue(flds3.get(0) == otherFirstNameFld);

		for (int i = 0; i < fieldDetails.length; i++) {
			IFieldDetail fieldDetail = flds1.get(i);

			assertEquals("check Field Name", "LAST-NAME", fieldDetail.getName());
			assertEquals("check Field pos", fieldDetails[i][0], fieldDetail.getPos());
			assertEquals("check Field Length", fieldDetails[i][1], fieldDetail.getLen());
			assertEquals("check Field Type", fieldDetails[i][2], fieldDetail.getType());
		}

		try {
			record.getUniqueField("LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields; should be only one", e.getMessage());
		}

		try {
			record.getUniqueField("LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found", e.getMessage());
		}
	}
	private void chkFields(String id, List<FieldDetail> flds, FieldDetail fld1, FieldDetail fld2, FieldDetail fld3) {
		assertTrue(id + "1", flds.get(0) == fld1);
		assertTrue(id + "2", flds.get(1) == fld2);
		assertTrue(id + "3", flds.get(2) == fld3);
	}
}
