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

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


public class TstOccursDepending29 extends TestCase {

//	private static final String MONTHS = "months";
//	private static final String WEEK_NO = "week-no";


	public void testPositionCalc1() throws Exception {
		try {
			tstPosition("OccursDependingOn29.cbl");
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

		System.out.println();
		boolean normal = copybookFile.endsWith("1.cbl");
		for (int i = 0; i < 16; i++) {
			System.out.print('*');
			for (int j = 0; j < 7; j++) {
				//System.out.println();
				//System.out.print("** line: " + i + " " + j);
				for (int week = 1; week < 4; week++) {
					for (int day = 1; day < 4; day++) {
						for (int hour = 1; hour < 5; hour++) {
							tstLine(ioBuilder.newLine(), i, j, week, day, hour, normal);
						}
					}
				}
			}
		}

	}
	

	private void tstLine(AbstractLine line, int purchaseCount, int salesCount, int week, int day, int hour, boolean normalPos) throws RecordException {
		String idm = purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		IFieldDetail weekNoFld = layout.getFieldFromName(Code.WEEK_NO_FIELD_NAME);
		IFieldDetail monthFld = layout.getFieldFromName(Code.MONTHS_FIELD_NAME);
		IFieldDetail weekCountFld = layout.getFieldFromName("week-of-month");
		IFieldDetail dayFld = layout.getFieldFromName("days");
		IFieldDetail hourFld = layout.getFieldFromName("hours");
		//IFieldDetail purchCountFld = layout.getFieldFromName("total-purchase-count");
		
		/** Setting the Occurs Depending fields !!! **/
		line.getFieldValue(monthFld).set(salesCount);
		line.getFieldValue(weekCountFld).set(week);
		line.getFieldValue(dayFld).set(day);
		line.getFieldValue(hourFld).set(hour);
		@SuppressWarnings("deprecation")
		int pos = hourFld.getEnd() + 1;
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);
		
		line.getFieldValue(weekNoFld).set(purchaseCount);
				
		check(l, line, layout.getFieldFromName("Location-Number"));
		check(l, line, layout.getFieldFromName("Location-Name"));
		
		l.add(monthFld);
		l.add(weekCountFld);
		l.add(dayFld);
		l.add(hourFld);

		for (int i = 0; i < salesCount; i++) {
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("week-sales (" + i + ", " + w +  ")"), pos);
				for (int d = 0; d < day; d++) {
					pos = check(l, line, layout.getFieldFromName("d-sales (" + i + ", " + w + ", " + d +  ")"), pos);
					for (int h = 0; h < hour; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
				}
			}
			for (int w = 0; w < week; w++) {
				for (int d = 0; d < day; d++) {
					for (int h = 0; h < hour; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
					pos = check(l, line, layout.getFieldFromName("d-value (" + i + ", " + w + ", " + d  + ")"), pos);
				}
				pos = check(l, line, layout.getFieldFromName("week-value (" + i + ", " + w + ")"), pos);
			}
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
		}

		pos = check(l, line, layout.getFieldFromName("total-sales"), pos);
		pos = check(l, line, layout.getFieldFromName(Code.WEEK_NO_FIELD_NAME), pos);		
	

		for (int i = 0; i < purchaseCount; i++) {
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("week-purch (" + i + ", " + w + ")"), pos);
				for (int d = 0; d < day; d++) {
					pos = check(l, line, layout.getFieldFromName("d-purch (" + i + ", " + w + ", " + d + ")"), pos);
					for (int h = 0; h < hour; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-purch (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
						pos = check(l, line, layout.getFieldFromName("daily-purch-val (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
					pos = check(l, line, layout.getFieldFromName("d-purch-val (" + i + ", " + w + ", " + d + ")"), pos);
				}
				pos = check(l, line, layout.getFieldFromName("week-purch-val (" + i + ", " + w + ")"), pos);
			}
			pos = check(l, line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);
			pos = check(l, line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		}

		pos = check(l, line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(l, line, layout.getFieldFromName("total-purchase-value"), pos);

		Code.checkFieldIteratore(line, idm, l);
//		FieldIterator fi = line.getFieldIterator(0);
//		int i = 0;
//		while (fi.hasNext()) {
//			AbstractFieldValue fv = fi.next();
//			FieldDetail fieldDetail = (FieldDetail) fv.getFieldDetail();
//			if (fieldDetail != l.get(i)) {
//				//System.out.println(fv.getFieldDetail().getName() + " \t" + l.get(i).getName() );
//				assertEquals(fieldDetail.getName(), l.get(i).getName() );
//			}
//			
//
//			assertEquals(idm + i + ") " + fieldDetail.getName(),
//					fieldDetail.calculateActualPosition(line), 
//					odCalc2.calculateActualPosition(line, fieldDetail.getDependingOnDtls(), fieldDetail.getPos()));
//
//			i += 1;
//		}
//
		//System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length);
	}

	private void check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld) throws RecordException {
		check(fieldList, line, fld, fld.getPos());
	}
	
	private int check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld, int pos) throws RecordException {		
		return Code.check(fieldList, line, fld, pos);
	}
}
