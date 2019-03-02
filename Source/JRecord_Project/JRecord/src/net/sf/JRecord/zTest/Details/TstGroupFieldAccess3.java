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

/**
 * Testing in-sequence version of GetFields methods;
 *
 * @author Bruce Martin
 *
 */
public class TstGroupFieldAccess3 extends TestCase {

	private String cobolCopybook
					= "        01  xx.\n"
					+ "            03 A.\n"
					+ "               05 B.\n"
					+ "                  07 Field-1    pic x(3).\n"
					+ "               05 C.\n"
					+ "                  07 Field-1    pic 9(4).\n"
					+ "            03 B.\n"
					+ "               05 A.\n"
					+ "                  07 Field-1    pic s9(6).\n"
					+ "            03 C.\n"
					+ "               05 A.\n"
					+ "                  07 Field-1    pic s9(3)v999.\n"
					+ "               05 D.\n"
					+ "                  07 Field-1    pic s9(5)v999.\n"

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
	private int recordIdx = 0;

	public void testGetFields() {
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = getFields("Field-1", 5);
		List<IFieldDetail> flds2 = getFields("Field-1", 4, "A");
		List<IFieldDetail> flds3 = getFields("Field-1", 2, "A", "B");
		List<IFieldDetail> flds4 = getFields("Field-1", 2, "B", "A");

		IFieldDetail fldAB = record.getUniqueFieldGroupsInSequence("Field-1", "A", "B");
		IFieldDetail fldBA = record.getUniqueFieldGroupsInSequence("Field-1", "B", "A");

		chkList("Flds 1/2", flds1, flds2);
		chkList("Flds 3/4", flds3, flds4);

		assertTrue(flds1.get(0) == flds3.get(0));
		assertTrue(flds1.get(2) == flds3.get(1));
		assertTrue(fldAB == flds3.get(0));
		assertTrue(fldBA == flds3.get(1));
	}


	public void testGetFieldsG() {
		RecordDetail record = schema.getRecord(recordIdx);
		List<IFieldDetail> flds1 = getFieldsG("Field-1", 5);
		List<IFieldDetail> flds2 = getFieldsG("Field-1", 4, "A");
		List<IFieldDetail> flds3 = getFieldsG("Field-1", 1, "A", "B");
		List<IFieldDetail> flds4 = getFieldsG("Field-1", 1, "B", "A");

		IFieldDetail fldAB = record.getUniqueFieldGroupsInSequence("Field-1", "A", "B");
		IFieldDetail fldBA = record.getUniqueFieldGroupsInSequence("Field-1", "B", "A");

		chkList("Flds 1/2", flds1, flds2);

		assertTrue(flds1.get(0) == flds3.get(0));
		assertTrue(flds1.get(2) == flds4.get(0));
		assertTrue(fldAB == flds3.get(0));
		assertTrue(fldBA == flds4.get(0));
	}

	private List<IFieldDetail> getFields(String fldName, int num, String... groupNames) {
		List<IFieldDetail> flds = schema.getRecord(recordIdx).getFields(fldName, groupNames);

		assertEquals(num, flds.size());

		return flds;
	}


	private List<IFieldDetail> getFieldsG(String fldName, int num, String... groupNames) {
		List<IFieldDetail> flds = schema.getRecord(recordIdx).getFieldsGroupsInSequence(fldName, groupNames);

		assertEquals(num, flds.size());

		return flds;
	}

	private void chkList(String id, List<IFieldDetail> list1, List<IFieldDetail> list2) {
		for (int i = 0; i < list2.size(); i++) {
			assertTrue(id + ": " + i, list1.get(i) == list2.get(i) );
		}
	}
}
