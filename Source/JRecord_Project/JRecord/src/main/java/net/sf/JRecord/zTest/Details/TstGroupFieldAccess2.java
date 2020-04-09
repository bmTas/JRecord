/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.zTest.Details;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

public class TstGroupFieldAccess2 extends TestCase {
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
			+ "         05 ZIP              PIC 9(5).\n"

			+ "     01 CUSTOMER-RECORD.\n     "
			+ "          05 EMPLOYEE-LIST.\n"
			+ "            10 CUSTOMER-NAME.\n"
			+ "               15 TITLE      PIC X(10)\n."
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 PARTNER-NAME.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 OTHERS.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n"
			;

	private LayoutDetail schema;
	{
		try {
			schema = TestCommonCode.getLayoutFromCobolStr(
							cobolCopybook, "COMPANY-RECORD",
							CopybookLoader.SPLIT_01_LEVEL, "", ICopybookDialects.FMT_INTEL);
		} catch (Exception e) {
			e.printStackTrace();
		}
	};
	private int recordIdx = schema.getRecordIndex("COMPANY-RECORD");

	@SuppressWarnings("deprecation")
	public void testFirstName1() {
		int[][] fieldDetails = {
				{46, 8, 0},
				{69, 8, 0},
				{102, 8, 0},
		};
		recordIdx = schema.getRecordIndex("COMPANY-RECORD");
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getFields("FIRST-NAME");
		List<IFieldDetail> flds2 = record.getFields("FIRST-NAME", "EMPLOYEE-LIST");
		List<IFieldDetail> flds3 = record.getFields("FIRST-NAME", "EMPLOYEE-LIST", "OTHERS");

		IFieldDetail presidentFirstNameFld = record.getUniqueField("FIRST-NAME", "PRESIDENT");
		IFieldDetail vicePresidentFirstNameFld = record.getUniqueField("FIRST-NAME", "VICE-PRESIDENT");
		IFieldDetail otherFirstNameFld = record.getUniqueField("FIRST-NAME", "OTHERS");

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


	public void testFirstName2() {
		int[][] fieldDetails = {
				{46, 8, 0},
				{69, 8, 0},
				{102, 8, 0},
		};
		recordIdx = schema.getRecordIndex("COMPANY-RECORD");
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("FIRST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "FIRST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "FIRST-NAME");

		IFieldDetail presidentFirstNameFld = record.getGroupField("PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld = record.getGroupField("OTHERS", "FIRST-NAME");

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
			record.getGroupField("FIRST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named FIRST-NAME; there should be only one", e.getMessage());
		}

		try {
			record.getGroupField("FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: FIRST-NAME~", e.getMessage());
		}
	}
	

	public void testFirstName3() {
		int[][] fieldDetails = {
				{46, 8, 0},
				{69, 8, 0},
				{102, 8, 0},
		};
		recordIdx = schema.getRecordIndex("COMPANY-RECORD");
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("FIRST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "FIRST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "FIRST-NAME");

		IFieldDetail presidentFirstNameFld     = record.getGroupField("COMPANY-RECORD", "PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("COMPANY-RECORD", "VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld         = record.getGroupField("COMPANY-RECORD", "OTHERS", "FIRST-NAME");

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
			record.getGroupField("COMPANY-RECORD", "FIRST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named FIRST-NAME; there should be only one", e.getMessage());
		}

		try {
			record.getGroupField("FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: FIRST-NAME~", e.getMessage());
		}

		try {
			record.getGroupField("COMPANY-RECORD", "FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: FIRST-NAME~", e.getMessage());
		}
	}

	

	public void testFirstName4() {
		int[][] fieldDetails = {
				{46, 8, 0},
				{69, 8, 0},
				{102, 8, 0},
		};
		recordIdx = schema.getRecordIndex("COMPANY-RECORD");
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("FIRST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "FIRST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "FIRST-NAME");

		IFieldDetail presidentFirstNameFld     = schema.getGroupField("COMPANY-RECORD", "PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = schema.getGroupField("COMPANY-RECORD", "VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld         = schema.getGroupField("COMPANY-RECORD", "OTHERS", "FIRST-NAME");

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
			schema.getGroupField("COMPANY-RECORD", "FIRST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named FIRST-NAME; there should be only one", e.getMessage());
		}


		try {
			schema.getGroupField("FIRST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found multiple fields named FIRST-NAME; there should be only one", e.getMessage());
		}

		try {
			schema.getGroupField("FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: .FIRST-NAME~", e.getMessage());
		}

		try {
			schema.getGroupField("COMPANY-RECORD", "FIRST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: FIRST-NAME~", e.getMessage());
		}
	}


	@SuppressWarnings("deprecation")
	public void testLastName1() {
		int[][] fieldDetails = {
				{31, 15, 0},
				{54, 15, 0},
				{87, 15, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getFields("LAST-NAME");
		List<IFieldDetail> flds2 = record.getFields("LAST-NAME", "EMPLOYEE-LIST");
		List<IFieldDetail> flds3 = record.getFields("LAST-NAME", "EMPLOYEE-LIST", "OTHERS");

		IFieldDetail presidentFirstNameFld = record.getUniqueField("LAST-NAME", "PRESIDENT");
		IFieldDetail vicePresidentFirstNameFld = record.getUniqueField("LAST-NAME", "VICE-PRESIDENT");
		IFieldDetail otherFirstNameFld = record.getUniqueField("LAST-NAME", "OTHERS");

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
	
	public void testLastName2() {
		int[][] fieldDetails = {
				{31, 15, 0},
				{54, 15, 0},
				{87, 15, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("LAST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "LAST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "LAST-NAME");

		IFieldDetail presidentFirstNameFld = record.getGroupField("PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherFirstNameFld = record.getGroupField("OTHERS", "LAST-NAME");

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
			record.getGroupField("LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named LAST-NAME; there should be only one", e.getMessage());
		}

		try {
			record.getGroupField("LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: LAST-NAME~", e.getMessage());
		}
	}

	public void testLastName3() {
		int[][] fieldDetails = {
				{31, 15, 0},
				{54, 15, 0},
				{87, 15, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("LAST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "LAST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "LAST-NAME");

		IFieldDetail presidentFirstNameFld = record.getGroupField("COMPANY-RECORD", "PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("COMPANY-RECORD", "VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherFirstNameFld = record.getGroupField("COMPANY-RECORD", "OTHERS", "LAST-NAME");

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
			record.getGroupField("LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named LAST-NAME; there should be only one", e.getMessage());
		}

		try {
			record.getGroupField("LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: LAST-NAME~", e.getMessage());
		}

		try {
			record.getGroupField("COMPANY-RECORD", "LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named LAST-NAME; there should be only one", e.getMessage());
		}

		try {
			record.getGroupField("COMPANY-RECORD", "LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: LAST-NAME~", e.getMessage());
		}
	}
	
	
	public void testLastName4() {
		int[][] fieldDetails = {
				{31, 15, 0},
				{54, 15, 0},
				{87, 15, 0},
		};
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = record.getGroupFields("LAST-NAME");
		List<IFieldDetail> flds2 = record.getGroupFields("EMPLOYEE-LIST", "LAST-NAME");
		List<IFieldDetail> flds3 = record.getGroupFields("EMPLOYEE-LIST", "OTHERS", "LAST-NAME");

		IFieldDetail presidentFirstNameFld = schema.getGroupField("COMPANY-RECORD", "PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentFirstNameFld = schema.getGroupField("COMPANY-RECORD", "VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherFirstNameFld = schema.getGroupField("COMPANY-RECORD", "OTHERS", "LAST-NAME");

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
			schema.getGroupField("LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found multiple fields named LAST-NAME; there should be only one", e.getMessage());
		}

		try {
			schema.getGroupField("LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: .LAST-NAME~", e.getMessage());
		}

		try {
			schema.getGroupField("COMPANY-RECORD", "LAST-NAME");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("Found 3 fields named LAST-NAME; there should be only one", e.getMessage());
		}

		try {
			schema.getGroupField("COMPANY-RECORD", "LAST-NAME~");
			throw new RuntimeException("Should not get here");
		} catch (Exception e) {
			assertEquals("No Field Found: LAST-NAME~", e.getMessage());
		}
	}

	private void chkFields(String id, List<IFieldDetail> flds, IFieldDetail fld1, IFieldDetail fld2, IFieldDetail fld3) {
		assertTrue(id + "1", flds.get(0) == fld1);
		assertTrue(id + "2", flds.get(1) == fld2);
		assertTrue(id + "3", flds.get(2) == fld3);
	}
}
