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

package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

/**
 * Test Occurs depending on with one nested occurs !!!
 * @author Bruce Martin
 *
 */
public class TstOccursDepending23 extends TestCase {

	private static final String MONTHS = "months";
	private static final String WEEK_NO = "week-no";

	public void testPositionCalc1() throws Exception {
		try {
			tstPosition("OccursDependingOn23.cbl");
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}
	

	
	private  void tstPosition(String copybookFile)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybookFile).getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);


		boolean normal = copybookFile.endsWith("1.cbl");
		for (int i = 0; i < 52; i++) {
			for (int j = 0; j < 12; j++) {
				for (int week = 1; week < 6; week++) {
						tstLine(ioBuilder.newLine(), i, j, week, 0, normal);
					
				}
			}
		}

	}
	

	private void tstLine(AbstractLine line, int purchaseCount, int salesCount, int week, int day, boolean normalPos) throws RecordException {
		LayoutDetail layout = line.getLayout();
		IFieldDetail weekNoFld = layout.getFieldFromName(WEEK_NO);
		IFieldDetail monthFld = layout.getFieldFromName(MONTHS);
		IFieldDetail weekCountFld = layout.getFieldFromName("week-of-month");
		IFieldDetail dayFld = layout.getFieldFromName("days");
		//IFieldDetail purchCountFld = layout.getFieldFromName("total-purchase-count");
		
			/** Setting the Occurs Depending fields !!! **/
		line.getFieldValue(monthFld).set(salesCount);
		line.getFieldValue(weekCountFld).set(week);
		line.getFieldValue(dayFld).set(day);
		@SuppressWarnings("deprecation")
		int pos = dayFld.getEnd() + 1;
		line.getFieldValue(weekNoFld).set(purchaseCount);
				
		check(line, layout.getFieldFromName("Location-Number"));
		check(line, layout.getFieldFromName("Location-Name"));

		for (int i = 0; i < salesCount; i++) {
			System.out.println("==> " + purchaseCount + ", sc=" + salesCount + ", week=" + week + " " + i);
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			if (i == 1 && salesCount==2 && week==2) {
				System.out.print('*');
			}
			for (int w = 0; w < week; w++) {
				pos = check(line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ")"), pos);
			}
			for (int w = 0; w < week; w++) {
				pos = check(line, layout.getFieldFromName("daily-value (" + i + ", " + w + ")"), pos);
			}
			for (int w = week; w < 5; w++) {
				assertFalse(line.getFieldValue("daily-sales (" + i + ", " + w + ")").isFieldInRecord());
				assertFalse(line.getFieldValue("daily-value (" + i + ", " + w + ")").isFieldInRecord());
			}
			pos = check(line, countFld, pos);
			pos = check(line, valueFld, pos);
		}
		
		for (int i = salesCount; i< 12; i++) {
			for (int w = 0; w < 5; w++) {
				assertFalse(line.getFieldValue("daily-sales (" + i + ", " + w + ")").isFieldInRecord());
				assertFalse(line.getFieldValue("daily-value (" + i + ", " + w + ")").isFieldInRecord());
			}
		}

		pos = check(line, layout.getFieldFromName("total-sales"), pos);
		pos = check(line, layout.getFieldFromName(WEEK_NO), pos);		
	

		for (int i = 0; i < purchaseCount; i++) {
			pos = check(line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);
			for (int w = 0; w < week; w++) {
				pos = check(line, layout.getFieldFromName("daily-purch-count (" + i + ", " + w + ")"), pos);
				pos = check(line, layout.getFieldFromName("daily-purch-value (" + i + ", " + w + ")"), pos);
			}
			pos = check(line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		}

		pos = check(line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(line, layout.getFieldFromName("total-purchase-value"), pos);

		
		for (int i = purchaseCount; i < 52; i++) {
			for (int w = 0; w < 5; w++) {
				String id = purchaseCount + ": " + i +", " + w;
				assertFalse(id, line.getFieldValue("daily-purch-count (" + i + ", " + w + ")").isFieldInRecord());
				assertFalse(id, line.getFieldValue("daily-purch-value (" + i + ", " + w + ")").isFieldInRecord());
			}
		}

		
//		System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length);
	}

	private void check(AbstractLine line, IFieldDetail fld) throws RecordException {
		check(line, fld, fld.getPos());
	}
	
	private int check(AbstractLine line, IFieldDetail fld, int pos) throws RecordException {
		String id = fld.getName();
		int calculatedPosition = fld.calculateActualPosition(line);
		if (pos != calculatedPosition) {
			calculatedPosition = fld.calculateActualPosition(line);
			assertEquals(id, pos, calculatedPosition);
		}
		int end = pos + fld.getLen() - 1;
		assertEquals(id, end, fld.calculateActualEnd(line));
		
		if (WEEK_NO.equalsIgnoreCase(fld.getName()) || MONTHS.equalsIgnoreCase(fld.getName())) {
			
		} else {
			for (int i = 0; i < 4; i++) {
				setAndCheck(line, fld, i);
			}
		}
		assertTrue(line.getFieldValue(fld).isFieldInRecord());

		return end + 1;
	}
	
	private void setAndCheck(AbstractLine line, IFieldDetail fld, int value) throws RecordException {
		
		AbstractFieldValue fieldValue = line.getFieldValue(fld);
		fieldValue.set(value);
		if (fieldValue.isNumeric()) {
			if (fld.getDecimal() == 0) {
				assertEquals(value, fieldValue.asInt());
			} else {
				assertEquals(Integer.toString(value) + ".00", fieldValue.asString());
			}
		} else {
			assertEquals(Integer.toString(value), fieldValue.asString());
		}
	}

}
