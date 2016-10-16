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

package net.sf.JRecord.zTest.Cobol.occursDependingOn.complex;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.occursDepending.ODCalculationComplex;

/**
 * Test Complex Occurs (2 levels) depending. The strategy is to:<ol>
 * <li>By choosing the array sizes carefully we can test Complex OD
 * against both Plain occurs and Simple occurs depending
 * <li>Testing Complex OD against Simple OD. 
 * </ol>  
 *  
 * 
 * @author Bruce Martin
 *
 */

public class TstOD03 extends TestCase {

	private static final int IOBUILDER_COUNT = 5;
	private static int IDX_COMPLEX_OD = 0;
	
	private static int IDX_NO_OD = IOBUILDER_COUNT - 1;
	
	

	/**
	 * Note: OD = Cobol Occurs-Depending-On clause
	 * 
	 * Test Complex OD (2 level OD's) against Simple OD and Plain arrays.
	 * This requires careful selection of the Complex-OD sizes
	 * 
	 * @throws Exception
	 */
	public void testPositionCalc1() throws Exception {
		String copybook = CCodeODC.getCobol101(true, CCodeODC.ODT_ARRAY_SIZE, 12, 5, 7);
		ICobolIOBuilder[] ioBuilder = new ICobolIOBuilder[IOBUILDER_COUNT];
		AbstractLine[] line = new AbstractLine[IOBUILDER_COUNT];
		long[] times = new long[IOBUILDER_COUNT];
		ioBuilder[IDX_COMPLEX_OD] = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "OD01")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);

		long currTime;


		for (int i = 0; i < 8; i+=1) {

			for (int j = 0; j <= 12; j+=1) {
				for (int week = 0; week < 6; week+=1) {
					String[] cbl = {
							CCodeODC.getCobol101( false, CCodeODC.ODT_ARRAY_SIZE, j, week, i),
							CCodeODC.getCobol101( false, CCodeODC.ODT_NON_ARRAY_SIZE, j, week, i),
							CCodeODC.getCobol101( true,  CCodeODC.ODT_STANDARD_ARRAY, j, week, i),
							CCodeODC.getCobol101( false, CCodeODC.ODT_STANDARD_ARRAY, j, week, i),
					};
					for (int ii = 0; ii < cbl.length; ii++) {
						ioBuilder[ii+1] = JRecordInterface1.COBOL
								.newIOBuilder( new StringReader(cbl[ii]), "OD01")
								.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
						
					}

					for (int ii = 0; ii < ioBuilder.length; ii++) {
						System.out.println("~~ test 1 " + i + "," + j + ", " + week + ", " +ii);
						line[ii] = ioBuilder[ii].newLine();
						
						currTime = System.currentTimeMillis();
						tstLine1(ii, line[ii], i, j, week, 0);
						times[ii] += System.currentTimeMillis() - currTime;
					}
					checkLineValues(ioBuilder, line);
				}
			}
			
			reportDetails("1", times);
		}
	}
	
	/**
	 * Test position Complex OD's against simple OD's
	 * 
	 * @throws Exception
	 */
	public void testPositionCalc2() throws Exception {
		String copybook = CCodeODC.getCobol101(true, CCodeODC.ODT_ARRAY_SIZE, 12, 5, 7);
		ICobolIOBuilder[] ioBuilder = new ICobolIOBuilder[2];
		AbstractLine[] line = new AbstractLine[2];
		long[] times = new long[2];
		ioBuilder[IDX_COMPLEX_OD] = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "OD01")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);

		long currTime;


		for (int i = 0; i < 10; i+=1) {
			for (int j = 0; j <= 12; j+=1) {
				for (int week = 0; week < 6; week+=1) {
					String cbl = CCodeODC.getCobol101( false, CCodeODC.ODT_ARRAY_SIZE, j, week, i);

					ioBuilder[1] = JRecordInterface1.COBOL
							.newIOBuilder( new StringReader(cbl), "OD01")
							.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE); 

					for (int ii = 0; ii < ioBuilder.length; ii++) {
						System.out.println("~~ test 2 " + i + "," + j + ", " + week + ", " +ii);
						
						currTime = System.currentTimeMillis();
						tstLine2(ii, (line[ii] = ioBuilder[ii].newLine()), i, j, week, 0);
						times[ii] += System.currentTimeMillis() - currTime;
					}
					checkLineValues(ioBuilder, line);
				}
			}
			reportDetails("2", times);
		}

	}


	private void reportDetails(String id, long[] times) {
		System.out.println();
		System.out.println();
		System.out.println("\t\t Calculation " + id );
		System.out.println();
		System.out.println("*********\t" + ODCalculationComplex.optCount 
				+ "\t" + ODCalculationComplex.normalCount
				+ "\t" + ((ODCalculationComplex.optCount * 100) 
					   /  (ODCalculationComplex.optCount  +ODCalculationComplex.normalCount)));
		System.out.println();
		for (long t: times) {
			System.out.print("\t" + t);
		}
		System.out.println();
	}


	private void checkLineValues(ICobolIOBuilder[] ioBuilder, AbstractLine[] line) {
		String s = line[ioBuilder.length - 1].getFullLine();
		for (int ii = 0; ii < ioBuilder.length - 1; ii++) {
			assertEquals(s, line[ii].getFullLine());
		}
	}


	private void tstLine1(int lineIdx, AbstractLine line, int purchaseCount, int salesCount, int week, int day) 
	throws RecordException {
		String idm = lineIdx + "-" +purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);

		int pos = tstLineHeader(l, line, salesCount, week, day);

		for (int i = 0; i < salesCount; i++) {
			pos = tstLineUpdateWeekFld1(l, line, "week-of-month1 (" + i + ")", layout, week, pos);
			
//			if (i == 1 && salesCount==2 && week==2) {
//				System.out.print('*');
//			}
			for (int w = 0; w < week; w++) {
				//System.out.print("daily-sales (" + i + ", " + w + ")");
				pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ")"), pos);
			}
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ")"), pos);
			}
			pos = tstLineSalesLoopTrailer(l, line, layout, lineIdx, week, idm, pos, i);
		}
		
		tstLineTrailer(l, line, layout, idm, lineIdx, purchaseCount, salesCount, week, pos);

		CCodeODC.checkFieldIteratore(line, idm, l);
	}
	

	private void tstLine2(int lineIdx, AbstractLine line, int purchaseCount, int salesCount, int week, int day) 
	throws RecordException {
		String idm = lineIdx + "-" +purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);

		int pos = tstLineHeader(l, line, salesCount, week, day);

		for (int i = 0; i < salesCount; i++) {
			int ww = (i + week) % 6; 
			pos = tstLineUpdateWeekFld1(l, line, "week-of-month1 (" + i + ")", layout, ww, pos);
			
//			if (i == 1 && salesCount==2 && week==2) {
//				System.out.print('*');
//			}
			for (int w = 0; w < ww; w++) {
				//System.out.print("daily-sales (" + i + ", " + w + ")");
				pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ")"), pos);
			}
			for (int w = 0; w < ww; w++) {
				pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ")"), pos);
			}
			pos = tstLineSalesLoopTrailer(l, line, layout, lineIdx, ww, idm, pos, i);
		}
		
		tstLineTrailer(l, line, layout, idm, lineIdx, purchaseCount, salesCount, week, pos);

		CCodeODC.checkFieldIteratore(line, idm, l);
	}





	private int tstLineHeader(ArrayList<IFieldDetail> l, AbstractLine line, int salesCount,
			int week, int day) {
		LayoutDetail layout = line.getLayout();

		IFieldDetail monthFld = layout.getFieldFromName(CCodeODC.MONTHS_FIELD_NAME);
		IFieldDetail weekCountFld = layout.getFieldFromName("week-of-month");
		IFieldDetail dayFld = layout.getFieldFromName("days");
		//IFieldDetail purchCountFld = layout.getFieldFromName("total-purchase-count");
		
			/** Setting the Occurs Depending fields !!! **/
		line.getFieldValue(monthFld).set(salesCount);
		line.getFieldValue(weekCountFld).set(week);
		line.getFieldValue(dayFld).set(day+1);
		@SuppressWarnings("deprecation")
		int pos = dayFld.getEnd() + 1;
				
		check(l, line, layout.getFieldFromName("Location-Number"));
		check(l, line, layout.getFieldFromName("Location-Name"));

		l.add(monthFld);
		l.add(weekCountFld);
		l.add(dayFld);
		return pos;
	}


	private int tstLineUpdateWeekFld1(ArrayList<IFieldDetail> l, AbstractLine line, String fieldName,
			LayoutDetail layout, int week, int pos) {
		IFieldDetail weekFld1 = layout.getFieldFromName(fieldName);
		line.getFieldValue(weekFld1).set(week);
		l.add(weekFld1);
		pos += weekFld1.getLen();
		return pos;
	}

	private int tstLineSalesLoopTrailer(ArrayList<IFieldDetail> l, AbstractLine line, LayoutDetail layout, int lineIdx,
			int week, String idm, int pos, int i) {
		if (lineIdx < IDX_NO_OD-1) { //== IDX_COMPLEX_OD ) {
			for (int w = week; w < 5; w++) {
				String msg = idm + i + ", " + w;
				//System.out.print("\t" + lineIdx);
				assertFalse(msg, line.getFieldValue("daily-sales (" + i + ", " + w + ")").isFieldInRecord());
				assertFalse(msg, line.getFieldValue("daily-value (" + i + ", " + w + ")").isFieldInRecord());
			}
		}
		IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
		IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
		pos = check(l, line, countFld, pos);
		pos = check(l, line, valueFld, pos);
		return pos;
	}




	private void tstLineTrailer(ArrayList<IFieldDetail> l, AbstractLine line, LayoutDetail layout, String idm,
			int lineIdx, int purchaseCount, int salesCount, int week, int pos) {
		IFieldDetail weekNoFld = layout.getFieldFromName(CCodeODC.WEEK_NO_FIELD_NAME);
		line.getFieldValue(weekNoFld).set(purchaseCount);

		if (lineIdx == IDX_COMPLEX_OD) {
			for (int i = salesCount; i< 12; i++) {
				for (int w = 0; w < 5; w++) {
					String msg = idm + i + ", " + w;
					assertFalse(msg, line.getFieldValue("daily-sales (" + i + ", " + w + ")").isFieldInRecord());
					assertFalse(msg, line.getFieldValue("daily-value (" + i + ", " + w + ")").isFieldInRecord());
				}
			}
		}

		pos = check(l, line, layout.getFieldFromName("total-sales"), pos);
		pos = check(l, line, layout.getFieldFromName(CCodeODC.WEEK_NO_FIELD_NAME), pos);		
	

		for (int i = 0; i < purchaseCount; i++) {
			IFieldDetail weekFld2 = layout.getFieldFromName("week-of-month2 (" + i + ")");
			pos = check(l, line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);

			line.getFieldValue(weekFld2).set(week);
			l.add(weekFld2);
			pos += weekFld2.getLen();
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("daily-purch-count (" + i + ", " + w + ")"), pos);
				pos = check(l, line, layout.getFieldFromName("daily-purch-value (" + i + ", " + w + ")"), pos);
			}
			pos = check(l, line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		}

//		System.out.println();
//		System.out.print("*** " + lineIdx + ", " + purchaseCount + ", " + week + ", " + lineIdx);
		pos = check(l, line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(l, line, layout.getFieldFromName("total-purchase-value"), pos);

		if (lineIdx == IDX_COMPLEX_OD) {
			for (int i = purchaseCount; i < 10; i++) {
//				System.out.println();
//				System.out.print(i + ": ");
				for (int w = 0; w < 5; w++) {
//					System.out.print(w + " ");
					String id = purchaseCount + ": " + i +", " + w;
					assertFalse(id, line.getFieldValue("daily-purch-count (" + i + ", " + w + ")").isFieldInRecord());
					assertFalse(id, line.getFieldValue("daily-purch-value (" + i + ", " + w + ")").isFieldInRecord());
				}
			}
		}
	}






	private void check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld) throws RecordException {
		check(fieldList, line, fld, fld.getPos());
	}
	
	private int check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld, int pos) throws RecordException {
		
//		int calculatedPosition = fld.calculateActualPosition(line);
//		if (pos != calculatedPosition) {
//			//calculatedPosition = fld.calculateActualPosition(line);
//			assertEquals(fld.getName(), pos, calculatedPosition);
//		}
		
		return CCodeODC.check(fieldList, line, fld, pos);

	}
}
