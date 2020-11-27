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
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
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
public class TstOccursDepending21 extends TestCase {

	private static final String MONTHS = "months";
	private static final String WEEK_NO = "week-no";

	public void testPositionCalc1() throws Exception {
		try {
			tstPosition1("OccursDependingOn21.cbl");
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}
	

	public void testPositionCalc2() throws Exception {
		try {
			tstPosition2("OccursDependingOn21.cbl");
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}
	
	
	private  void tstPosition1(String copybookFile)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybookFile).getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME);
		tstPosition(copybookFile, ioBuilder);

	}


	
	private  void tstPosition2(String copybookFile)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybookFile).getFile();
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(copybookFileName).setDialect(ICopybookDialects.FMT_MAINFRAME);
		tstPosition(copybookFile, ioBuilder);

	}

	/**
	 * @param copybookFile
	 * @param ioBuilder
	 * @throws RecordException
	 * @throws IOException
	 */
	private void tstPosition(String copybookFile, ICobolIOBuilder ioBuilder)
			throws RecordException, IOException {
		ioBuilder	.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);


		boolean normal = copybookFile.endsWith("1.cbl");
		for (int i = 0; i < 52; i++) {
			int j = 12;
//			for (int j = 6; j <= 12; j++) {
			for (int week = 1; week < 6; week++) {
				tstLine(ioBuilder.newLine(), i, j, week, 0, normal);
			}
//			}
		}
	}
	

	private void tstLine(AbstractLine line, int purchaseCount, int salesCount, int week, int day, boolean normalPos) throws RecordException {
		String idm = purchaseCount + "~" + salesCount + ":" + week + " ~ ";
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
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);
		
		
		line.getFieldValue(weekNoFld).set(purchaseCount);
				
		check(l, line, layout.getFieldFromName("Location-Number"));
		check(l, line, layout.getFieldFromName("Location-Name"));
		
		l.add(monthFld);
		l.add(weekCountFld);
		l.add(dayFld);

		for (int i = 0; i < salesCount; i++) {
			
			System.out.println("==> " + purchaseCount + ", sc=" + salesCount + ", week=" + week + " " + i);
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ")"), pos);
			}
			if (i == 1) {
				System.out.print('*');
			}
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
			for (int w = week; w < 5; w++) {
				assertFalse("Week: " + week + " " +i +", " + w, 
						line.isFieldInLine(layout.getFieldFromName("daily-sales (" + i + ", " + w + ")")));
			}
		}
//		for (int i =  salesCount; i < 12; i++) {			
//			assertFalse(line.isFieldInLine(layout.getFieldFromName("sales-count (" + i + ")")));
//			assertFalse(line.isFieldInLine(layout.getFieldFromName("sales-value (" + i + ")")));
//			for (int w = 0; w < 6; w++) {
//				assertFalse("SalesCount: " + salesCount + " " + i + ", " + w, 
//						line.isFieldInLine(layout.getFieldFromName("daily-sales (" + i + ", " + w + ")")));
//			}
//		}

		pos = check(l, line, layout.getFieldFromName("total-sales"), pos);
		pos = check(l, line, layout.getFieldFromName(WEEK_NO), pos);		
	

		for (int i = 0; i < purchaseCount; i++) {
			IFieldDetail countFld = layout.getFieldFromName("purchase-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("purchase-value (" + i + ")");
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
		}
		

		for (int i = purchaseCount; i < 52; i++) {
			IFieldDetail countFld = layout.getFieldFromName("purchase-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("purchase-value (" + i + ")");

			assertFalse(line.isFieldInLine(countFld));
			assertFalse(line.isFieldInLine(valueFld));
		}
		
		

		pos = check(l, line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(l, line, layout.getFieldFromName("total-purchase-value"), pos);

		Code.checkFieldIteratore(line, idm, l);

//		//System.out.println();
//		//System.out.println();
//		int i = 0;
//		while (fi.hasNext()) {
//			AbstractFieldValue fv = fi.next();
//			if (fv.getFieldDetail() != l.get(i)) {
//				//System.out.println(fv.getFieldDetail().getName() + " \t" + l.get(i).getName() );
//				assertEquals(fv.getFieldDetail().getName(), l.get(i).getName() );
//			}
//			i += 1;
//		}

		
//		System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length);
	}

	private void check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld) throws RecordException {
		check(fieldList, line, fld, fld.getPos());
	}
	
	private int check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld, int pos) throws RecordException {
		String id = fld.getName();
		fieldList.add(fld);
		
		assertEquals(id, pos, fld.calculateActualPosition(line));
		int end = pos + fld.getLen() - 1;
		assertEquals(id, end, fld.calculateActualEnd(line));
		
		if (WEEK_NO.equalsIgnoreCase(fld.getName()) || MONTHS.equalsIgnoreCase(fld.getName())) {
			
		} else {
			for (int i = 0; i < 4; i++) {
				setAndCheck(line, fld, i);
			}
		}
		assertTrue(line.isFieldInLine(fld));

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
