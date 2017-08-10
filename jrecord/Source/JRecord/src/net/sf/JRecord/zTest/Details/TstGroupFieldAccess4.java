package net.sf.JRecord.zTest.Details;

import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TestCommonCode;

public class TstGroupFieldAccess4 extends TestCase {

	private String employeeList
	= "               10 PRESIDENT.\n"
	+ "                  15 LAST-NAME  PIC X(15).\n"
	+ "                  15 FIRST-NAME PIC X(8).\n"
	+ "               10 VICE-PRESIDENT.\n"
	+ "                  15 LAST-NAME  PIC X(15).\n"
	+ "                  15 FIRST-NAME PIC X(8).\n"
	+ "               10 OTHERS.\n"
	+ "                  15 TITLE      PIC X(10).\n"
	+ "                  15 LAST-NAME  PIC X(15).\n"
	+ "                  15 FIRST-NAME PIC X(8).\n";

	private String cobolCopybook
	= "      01 COMPANY-RECORD.\n"
	+ "         05 COMPANY-NAME     PIC X(30).\n"
    + "         05 Office-Holders.\n"
	+ "            07 EMPLOYEE-LIST.\n"
	+ "               10 TITLE      PIC X(10).\n"
	+ "               10 LAST-NAME  PIC X(15).\n"
	+ "               10 FIRST-NAME PIC X(8).\n"
	+ employeeList
	+ "         05 ADDRESS          PIC X(15).\n"
	+ "         05 CITY             PIC X(15).\n"
	+ "         05 STATE            PIC XX.\n"
	+ "         05 ZIP              PIC 9(5).\n";

	private int recordIdx = 0;
	private LayoutDetail schema, schema1;
	{
		try {
			schema = TestCommonCode.getLayoutFromCobolStr(
							cobolCopybook, "COMPANY-RECORD",
							CopybookLoader.SPLIT_NONE, "", ICopybookDialects.FMT_INTEL);
			schema1 = TestCommonCode.getLayoutFromCobolStr(
					  "      01 RECORD-01     pic x(20).\n"
					+ "      01 RECORD-02     pic x(20).\n"
					+ cobolCopybook
					+ "      01 RECORD-04.\n"
					+ "         05 Office-Holders.\n"
					+ "            07 EMPLOYEE-LIST.\n"
					+ employeeList
					, 				
					"COMPANY-RECORD",
					CopybookLoader.SPLIT_01_LEVEL, "", ICopybookDialects.FMT_INTEL);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void testRecordGetFields() {
		checkRecord(schema.getRecord(recordIdx), 4, 2);
		
		checkRecord(schema1.getRecord(2), 4, 2);
	
		checkRecord(schema1.getRecord(3), 3, 1);
	}

	private void checkRecord(RecordDetail record, int expected, int expected2) {
		List<IFieldDetail> gfs = record.getGroupFields("Office-Holders", "First-Name");
		assertEquals(expected, gfs.size());
		
		gfs = record.getGroupFields("Office-Holders", "EMPLOYEE-LIST", "First-Name");
		assertEquals(expected, gfs.size());
		
		gfs = record.getGroupFields("Office-Holders", "EMPLOYEE-LIST", "Last-Name");
		assertEquals(expected, gfs.size());
		
		gfs = record.getGroupFields("Office-Holders", "EMPLOYEE-LIST", "TITLE");
		assertEquals(expected2, gfs.size());
	}
	
	


	public void testRecordGetField() {

		checkGetField(schema.getRecord(0), "First-Name", 56, 8, 4);
		checkGetField(schema.getRecord(0), "Last-Name", 41, 15, 4 );
		checkGetField(schema.getRecord(0), "TITLE", 31, 10, 2);
		checkGetField(schema1.getRecord(2), "First-Name", 56, 8, 4);
		checkGetField(schema1.getRecord(2), "Last-Name", 41, 15, 4);
		checkGetField(schema1.getRecord(2), "TITLE", 31, 10, 2);
		
		
		
		try {
			schema.getRecord(0).getGroupField("Office-Holders", "xxx");
			throw new RuntimeException("Previous statment should fail !!!");
		} catch (RecordException re) {
			assertEquals("No Field Found: xxx", re.getMessage());
		}

	}
	

	protected void checkGetField(RecordDetail record, String fldName, int pos, int len, int fldCount) {
		IFieldDetail fld = record.getGroupField("Office-Holders", "EMPLOYEE-LIST", fldName);
		
		assertTrue(fld != null);
		assertEquals(fldName.toUpperCase(), fld.getName());
		assertEquals(pos, fld.getPos());
		assertEquals(len, fld.getLen());
		
		try {
			fld = record.getGroupField("Office-Holders", fldName);
			throw new RuntimeException("Previous statment should fail !!!");
		} catch (RecordException re) {
			assertEquals("Found " + fldCount + " fields named " + fldName + "; there should be only one", re.getMessage());
		}
	}

	
	public void testLayoutGetField() {

		checkGetField(schema, "First-Name", 56, 8);
		checkGetField(schema, "Last-Name", 41, 15);
		checkGetField(schema, "TITLE", 31, 10);
		checkGetField(schema1, "First-Name", 56, 8);
		checkGetField(schema1, "Last-Name", 41, 15);
		checkGetField(schema1, "TITLE", 31, 10);
		
		try {
			schema.getGroupField("Office-Holders", "xxx");
			throw new RuntimeException("Previous statment should fail !!!");
		} catch (RecordException re) {
			assertEquals("No Field Found: .Office-Holders.xxx", re.getMessage());
		}

	}

	protected void checkGetField(LayoutDetail schema, String fldName, int pos, int len) {
		IFieldDetail fld = schema.getGroupField("Office-Holders", "EMPLOYEE-LIST", fldName);
		
		assertTrue(fld != null);
		assertEquals(fldName.toUpperCase(), fld.getName());
		assertEquals(pos, fld.getPos());
		assertEquals(len, fld.getLen());
		
		try {
			fld = schema.getGroupField("Office-Holders", fldName);
			throw new RuntimeException("Previous statment should fail !!!");
		} catch (RecordException re) {
			assertEquals("Found multiple fields named " + fldName + "; there should be only one", re.getMessage());
		}
	}
	
	
}