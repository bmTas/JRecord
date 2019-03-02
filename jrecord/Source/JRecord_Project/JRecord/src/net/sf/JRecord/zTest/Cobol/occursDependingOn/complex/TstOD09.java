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

import java.io.IOException;
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
 * 
 * Note: OD = Cobol Occurs-Depending-On clause
 * 
 * Test Complex Occurs (4 level) depending. The strategy is to:<ol>
 * <li>By choosing the array sizes carefully we can test Complex OD
 * against both Plain occurs and Simple occurs depending
 * <li>Testing Complex OD against Simple OD. This still requires
 * selection of OD array sizes but allows greater choices than in option 1.
 * <li>Just Testing Complex OD's
 * </ol>  
 *  
 * 
 * @author Bruce Martin
 *
 */
public class TstOD09 extends TestCase {

	private static final int IOBUILDER_COUNT = 7;
	private static int IDX_COMPLEX_OD = 0;
	
//	private static int IDX_NO_OD = IOBUILDER_COUNT - 1;
//	
	
	/*
	 * Testing Complex OD against
	 *   * Simple OD
	 *   * Normal arrays 
	 */

	public void testPositionCalc10() throws Exception {
		checkForPurchNum1(0);
	}


	public void testPositionCalc11() throws Exception {
		checkForPurchNum1( 1);
	}


	public void testPositionCalc12() throws Exception {

		checkForPurchNum1( 3);
		checkForPurchNum1( 4);
	}


	/*
	 *    Testing Complex OD's against Simple OD's
	 */
	public void testPositionCalc20() throws Exception {
		checkForPurchNum2(0);
	}


	public void testPositionCalc21() throws Exception {
		checkForPurchNum2( 1);
	}


	public void testPositionCalc22() throws Exception {

		checkForPurchNum2( 3);
		checkForPurchNum2( 4);
	}

	
	/*
	 * Testing with all Complex OD and different size arrays at every point
	 */

	public void testPositionCalc30() throws Exception {
		checkForPurchNum3(0);
	}


	public void testPositionCalc31() throws Exception {
		checkForPurchNum3( 1);
	}


	public void testPositionCalc32() throws Exception {

		checkForPurchNum3( 3);
		checkForPurchNum3( 4);
	}

	
	private void checkForPurchNum1(int purchNum)
		throws IOException {
		String copybook = CCodeODC.getCobol111(true, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_ARRAY_SIZE, 12, 5, 7, 4, 5);
		ICobolIOBuilder[] ioBuilder = new ICobolIOBuilder[IOBUILDER_COUNT];
		AbstractLine[] line = new AbstractLine[IOBUILDER_COUNT];
		long[] times = new long[IOBUILDER_COUNT];
		ioBuilder[IDX_COMPLEX_OD] = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "OD01")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		long currTime;
		for (int salesCnt = 0; salesCnt <= 8; salesCnt+=2) {
			for (int week = 0; week < 5; week+=1) {
				for (int day = 0; day < 4; day+=1) {
					for (int hour = 0; hour < 4; hour+=1) {
						String[] cbl = {
								CCodeODC.getCobol111( false, CCodeODC.ODT_NON_ARRAY_SIZE, CCodeODC.ODT_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( false, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_NON_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( false, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_PARENT_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( false, CCodeODC.ODT_NON_ARRAY_SIZE, CCodeODC.ODT_NON_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( true,  CCodeODC.ODT_STANDARD_ARRAY, CCodeODC.ODT_STANDARD_ARRAY,
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( false, CCodeODC.ODT_STANDARD_ARRAY, CCodeODC.ODT_STANDARD_ARRAY,
										salesCnt, week, purchNum, day, hour),
						};
						for (int ii = 0; ii < cbl.length; ii++) {
							ioBuilder[ii+1] = JRecordInterface1.COBOL
									.newIOBuilder( new StringReader(cbl[ii]), "OD01")
									.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
							
						}
	
						for (int ii = ioBuilder.length-1; ii >= 0; ii--) {
							String iid = purchNum + "," + salesCnt + ", " + week + ", "
									   + day + ": " + hour + ", " +ii;
							System.out.println("~~ test 1 " + iid);
							line[ii] = ioBuilder[ii].newLine();
							
							if (iid.startsWith("1,0, 1, 1: 1, 5")) {
								System.out.print('*');
							}
							currTime = System.currentTimeMillis();
							tstLine1(ii, line[ii], purchNum, salesCnt, week, day, hour);
							times[ii] += System.currentTimeMillis() - currTime;
						}
						checkLineValues(ioBuilder, line);
					}
				}
			}
		}
		
		reportDetails("1", times);
	}
	

	private void checkForPurchNum2(int purchNum)
		throws IOException {
		String copybook = CCodeODC.getCobol111(true, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_ARRAY_SIZE, 12, 5, 7, 4, 5);
		ICobolIOBuilder[] ioBuilder = new ICobolIOBuilder[3];
		AbstractLine[] line = new AbstractLine[ioBuilder.length];
		long[] times = new long[ioBuilder.length];
		ioBuilder[IDX_COMPLEX_OD] = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "OD01")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		long currTime;
		for (int salesCnt = 0; salesCnt <= 8; salesCnt+=2) {
			for (int week = 0; week < 5; week+=1) {
				for (int day = 0; day < 4; day+=1) {
					for (int hour = 0; hour < 4; hour+=1) {
						String[] cbl = {
								CCodeODC.getCobol111( false, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_PARENT_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
								CCodeODC.getCobol111( false, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_ARRAY_SIZE, 
										salesCnt, week, purchNum, day, hour),
						};
						for (int ii = 0; ii < cbl.length; ii++) {
							ioBuilder[ii+1] = JRecordInterface1.COBOL
									.newIOBuilder( new StringReader(cbl[ii]), "OD01")
									.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
							
						}
	
						for (int ii = ioBuilder.length-1; ii >= 0; ii--) {
							String iid = purchNum + "," + salesCnt + ", " + week + ", "
									   + day + ": " + hour + ", " +ii;
							System.out.println("~~ test 1 " + iid);
							line[ii] = ioBuilder[ii].newLine();
							
							if (iid.startsWith("1,0, 1, 1: 1, 5")) {
								System.out.print('*');
							}
							currTime = System.currentTimeMillis();
							tstLine2(ii, line[ii], purchNum, salesCnt, week, day, hour);
							times[ii] += System.currentTimeMillis() - currTime;
						}
						checkLineValues(ioBuilder, line);
					}
				}
			}
		}
		
		reportDetails("2", times);
	}

	

	private void checkForPurchNum3(int i)
		throws IOException {
		String copybook = CCodeODC.getCobol111(true, CCodeODC.ODT_ARRAY_SIZE, CCodeODC.ODT_ARRAY_SIZE, 12, 5, 7, 4, 5);
		ICobolIOBuilder[] ioBuilder = new ICobolIOBuilder[1];
		AbstractLine[] line = new AbstractLine[ioBuilder.length];
		long[] times = new long[ioBuilder.length];
		ioBuilder[IDX_COMPLEX_OD] = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(copybook), "OD01")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		long currTime;
		for (int salesCnt = 0; salesCnt <= 8; salesCnt+=2) {
			for (int week = 0; week < 5; week+=1) {
				for (int day = 0; day < 4; day+=1) {
					for (int hour = 0; hour < 4; hour+=1) {
						for (int ii = ioBuilder.length-1; ii >= 0; ii--) {
							String iid = i + "," + salesCnt + ", " + week + ", "
									   + day + ": " + hour + ", " +ii;
							System.out.println("~~ test 1 " + iid);
							line[ii] = ioBuilder[ii].newLine();
							
							if (iid.startsWith("1,0, 1, 1: 1, 5")) {
								System.out.print('*');
							}
							currTime = System.currentTimeMillis();
							tstLine3(ii, line[ii], i, salesCnt, week, day, hour);
							times[ii] += System.currentTimeMillis() - currTime;
						}
						checkLineValues(ioBuilder, line);
					}
				}
			}
		}
		
		reportDetails("2", times);
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


	private void tstLine1(int lineIdx, AbstractLine line, int purchaseCount, int salesCount, int week, int day, int hour) 
	throws RecordException {
		String idm = lineIdx + "-" +purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);

		int pos = tstLineHeader(l, line, salesCount, week, day, hour);

		for (int i = 0; i < salesCount; i++) {
			pos = tstLineUpdateWeekFld1(l, line, i , week, day, hour, pos);
			
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}
			for (int w = 0; w < week; w++) {
				pos = check(l, line, layout.getFieldFromName("week-sales (" + i + ", " + w +  ")"), pos);
				pos += updateField(l, line, "days-11 (" + i + ", " + w + ")", day);

				for (int d = 0; d < day; d++) {
					pos = check(l, line, layout.getFieldFromName("d-sales (" + i + ", " + w  + ", " + d +  ")"), pos);
					pos += updateField(l, line, "hours-11 (" + i + ", " + w  + ", " + d + ")", hour);
					for (int h = 0; h < hour; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
				}
			}
			for (int w = 0; w < week; w++) {
				pos += updateField(l, line, "days-12 (" + i + ", " + w + ")", day);
				for (int d = 0; d < day; d++) {
					pos += updateField(l, line, "hours-12 (" + i + ", " + w + ", " + d + ")", hour);
					for (int h = 0; h < hour; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
					pos = check(l, line, layout.getFieldFromName("d-value (" + i + ", " + w + ", " + d  + ")"), pos);
				}
				pos = check(l, line, layout.getFieldFromName("week-value (" + i + ", " + w + ")"), pos);
			}
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
		}
		
		tstLineTrailer1(l, line, idm, lineIdx, purchaseCount, salesCount, week, day, hour, pos);

		CCodeODC.checkFieldIteratore(line, idm, l);
	}
	

	private void tstLine2(int lineIdx, AbstractLine line, int purchaseCount, int salesCount, int week, int day, int hour) 
	throws RecordException {
		String idm = lineIdx + "-" +purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);

		int pos = tstLineHeader(l, line, salesCount, week, day, hour);

		for (int i = 0; i < salesCount; i++) {
			int ww = (week + i) % 5;
			int dd = (day + ww) % 4;
			int hh = (hour + dd + i) % 5;
			pos = tstLineUpdateWeekFld1(l, line, i , ww, dd, hh, pos);
			
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}
			for (int w = 0; w < ww; w++) {
				pos = check(l, line, layout.getFieldFromName("week-sales (" + i + ", " + w +  ")"), pos);
				pos += updateField(l, line, "days-11 (" + i + ", " + w + ")", dd);

				for (int d = 0; d < dd; d++) {
					pos = check(l, line, layout.getFieldFromName("d-sales (" + i + ", " + w  + ", " + d +  ")"), pos);
					pos += updateField(l, line, "hours-11 (" + i + ", " + w  + ", " + d + ")", hh);
					for (int h = 0; h < hh; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
				}
			}
			for (int w = 0; w < ww; w++) {
				pos += updateField(l, line, "days-12 (" + i + ", " + w + ")", dd);
				for (int d = 0; d < dd; d++) {
					pos += updateField(l, line, "hours-12 (" + i + ", " + w + ", " + d + ")", hh);
					for (int h = 0; h < hh; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
					pos = check(l, line, layout.getFieldFromName("d-value (" + i + ", " + w + ", " + d  + ")"), pos);
				}
				pos = check(l, line, layout.getFieldFromName("week-value (" + i + ", " + w + ")"), pos);
			}
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
		}
		
		tstLineTrailer2(l, line, idm, lineIdx, purchaseCount, salesCount, week, day, hour, pos);

		CCodeODC.checkFieldIteratore(line, idm, l);
	}


	private void tstLine3(int lineIdx, AbstractLine line, int purchaseCount, int salesCount, int week, int day, int hour) 
	throws RecordException {
		String idm = lineIdx + "-" +purchaseCount + "~" + salesCount + ":" + week + " ~ ";
		LayoutDetail layout = line.getLayout();
		ArrayList<IFieldDetail> l = new ArrayList<IFieldDetail>(200);

		int pos = tstLineHeader(l, line, salesCount, week, day, hour);

		for (int i = 0; i < salesCount; i++) {
			int ww = (week + i) % 5;
			pos = tstLineUpdateWeekFld1(l, line, i , ww, day, hour, pos);
			
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}
			for (int w = 0; w < ww; w++) {
				int dd = (day + w) % 4;
				pos = check(l, line, layout.getFieldFromName("week-sales (" + i + ", " + w +  ")"), pos);
				pos += updateField(l, line, "days-11 (" + i + ", " + w + ")", dd);

				for (int d = 0; d < dd; d++) {
					int hh = (hour + d + i) % 5;
					pos = check(l, line, layout.getFieldFromName("d-sales (" + i + ", " + w  + ", " + d +  ")"), pos);
					pos += updateField(l, line, "hours-11 (" + i + ", " + w  + ", " + d + ")", hh);
					for (int h = 0; h < hh; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-sales (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
				}
			}
			for (int w = 0; w < ww; w++) {
				int dd = (day + w + 3) % 4;
				pos += updateField(l, line, "days-12 (" + i + ", " + w + ")", dd);
				for (int d = 0; d < dd; d++) {
					int hh = (hour + d + 3 + i) % 5;
					pos += updateField(l, line, "hours-12 (" + i + ", " + w + ", " + d + ")", hh);
					for (int h = 0; h < hh; h++) {
						pos = check(l, line, layout.getFieldFromName("daily-value (" + i + ", " + w + ", " + d + ", " + h + ")"), pos);
					}
					pos = check(l, line, layout.getFieldFromName("d-value (" + i + ", " + w + ", " + d  + ")"), pos);
				}
				pos = check(l, line, layout.getFieldFromName("week-value (" + i + ", " + w + ")"), pos);
			}
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			pos = check(l, line, countFld, pos);
			pos = check(l, line, valueFld, pos);
		}
		
		tstLineTrailer3(l, line, idm, lineIdx, purchaseCount, salesCount, week, day, hour, pos);

		CCodeODC.checkFieldIteratore(line, idm, l);
	}

	private int updateField(List<IFieldDetail> l, AbstractLine line, String fieldName, int value) {
		
		IFieldDetail fld = line.getLayout().getFieldFromName(fieldName);
		if (fld == null) {
			//System.out.println("Can not retrieve: " + fieldName);
			fld = line.getLayout().getFieldFromName(fieldName);
			throw new RuntimeException("Can not retrieve: " + fieldName);
		}
		line.getFieldValue(fld).set(value);
		l.add(fld);
		
		return fld.getLen();
		
	}





	private int tstLineHeader(ArrayList<IFieldDetail> l, AbstractLine line, int salesCount,
			int week, int day, int hour) {
		LayoutDetail layout = line.getLayout();

		IFieldDetail monthFld = layout.getFieldFromName(CCodeODC.MONTHS_FIELD_NAME);
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
				
		check(l, line, layout.getFieldFromName("Location-Number"));
		check(l, line, layout.getFieldFromName("Location-Name"));

		l.add(monthFld);
		l.add(weekCountFld);
		l.add(dayFld);
		l.add(hourFld);

		return pos;
	}


	private int tstLineUpdateWeekFld1(ArrayList<IFieldDetail> l, AbstractLine line, int i,
			 int week, int day, int hour, int pos) {
		
		LayoutDetail layout = line.getLayout();
		IFieldDetail weekFld1 = layout.getFieldFromName("week-of-month-1 (" + i + ")");
		IFieldDetail dayFld = layout.getFieldFromName("days-1 (" + i + ")");
		IFieldDetail hourFld = layout.getFieldFromName("hours-1 (" + i + ")");
		
		line.getFieldValue(weekFld1).set(week);
		line.getFieldValue(dayFld).set(day);
		line.getFieldValue(hourFld).set(hour);
		
		l.add(weekFld1);
		l.add(dayFld);
		l.add(hourFld);
		
		pos += weekFld1.getLen() + dayFld.getLen() + hourFld.getLen();
		return pos;
	}



	private void tstLineTrailer1(List<IFieldDetail> l, AbstractLine line, String idm,
			int lineIdx, int purchaseCount, int salesCount, int week, int days, int hours, int pos) {

		pos = tstLineTrailer1Start(l, line, purchaseCount, pos);

		for (int i = 0; i < purchaseCount; i++) {		
			pos = checkPurchases(l, line, week, days, hours, pos, i);
		}
		
		tstLineTrailer1End(l, line, pos);
	}

	

	private void tstLineTrailer2(List<IFieldDetail> l, AbstractLine line, String idm,
			int lineIdx, int purchaseCount, int salesCount, int week, int days, int hours, int pos) {

		pos = tstLineTrailer1Start(l, line, purchaseCount, pos);

		for (int i = 0; i < purchaseCount; i++) {		
			int ww = (week + i +3) % 5;
			int dd = (days + ww) % 4;
			int hh = (hours + dd + i) % 5;
			pos = checkPurchases(l, line, ww, dd, hh, pos, i);
		}
		
		tstLineTrailer1End(l, line, pos);
	}


	private void tstLineTrailer3(List<IFieldDetail> l, AbstractLine line, String idm,
			int lineIdx, int purchaseCount, int salesCount, int week, int days, int hours, int pos) {

		pos = tstLineTrailer1Start(l, line, purchaseCount, pos);

		for (int i = 0; i < purchaseCount; i++) {		
			int ww = (week + i +3) % 5;
			int dd = (days + ww) % 4;
			int hh = (hours + dd + i) % 5;
			pos = checkPurchases3(l, line, ww, dd, hh, pos, i);
		}
		
		tstLineTrailer1End(l, line, pos);
	}


	private int tstLineTrailer1Start(List<IFieldDetail> l, AbstractLine line, int purchaseCount, int pos) {
		LayoutDetail layout = line.getLayout();
		
//		if (lineIdx == IDX_COMPLEX_OD) {
//			for (int i = salesCount; i< 12; i++) {
//				for (int w = 0; w < 5; w++) {
//					String msg = idm + i + ", " + w;
//					assertFalse(msg, line.getFieldValue("daily-sales (" + i + ", " + w + ")").isFieldInRecord());
//					assertFalse(msg, line.getFieldValue("daily-value (" + i + ", " + w + ")").isFieldInRecord());
//				}
//			}
//		}

		pos = check(l, line, layout.getFieldFromName("total-sales"), pos);
		pos += updateField(l, line, CCodeODC.WEEK_NO_FIELD_NAME, purchaseCount);
		return pos;
	}
	
	
	private void tstLineTrailer1End(List<IFieldDetail> l, AbstractLine line, int pos) {
		
		LayoutDetail layout = line.getLayout();

//		System.out.println();
//		System.out.print("*** " + lineIdx + ", " + purchaseCount + ", " + week + ", " + lineIdx);
		pos = check(l, line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(l, line, layout.getFieldFromName("total-purchase-value"), pos);

//		if (lineIdx == IDX_COMPLEX_OD) {
//			for (int i = purchaseCount; i < 10; i++) {
////				System.out.println();
////				System.out.print(i + ": ");
//				for (int w = 0; w < 5; w++) {
////					System.out.print(w + " ");
//					String id = purchaseCount + ": " + i +", " + w;
//					assertFalse(id, line.getFieldValue("daily-purch-count (" + i + ", " + w + ")").isFieldInRecord());
//					assertFalse(id, line.getFieldValue("daily-purch-value (" + i + ", " + w + ")").isFieldInRecord());
//				}
//			}
//		}
	}


	private int checkPurchases(List<IFieldDetail> l, AbstractLine line, int week, int days, int hours, int pos,
			int i) {
		
		LayoutDetail layout = line.getLayout();
		pos += updateField(l, line, "week-of-month-2 (" + i + ")", week);
		pos += updateField(l, line, "days-2 (" + i + ")", days);
		pos += updateField(l, line, "hours-2 (" + i + ")", hours);

		for (int w = 0; w < week; w++) {
			pos = check(l, line, layout.getFieldFromName("week-purch (" + i + ", " + w + ")"), pos);
			pos += updateField(l, line, "days-21 (" + i + ", " + w + ")", days);
			for (int d = 0; d < days; d++) {
				pos = check(l, line, layout.getFieldFromName("d-purch (" + i + ", " + w + ", " + d + ")"), pos);
				pos += updateField(l, line, "hours-21 (" + i + ", " + w + ", " + d + ")", hours);
				
				for (int h = 0; h < hours; h++) {
					pos = check(l, line, 
							layout.getFieldFromName("daily-purch (" + i + ", " + w + ", " + d + ", " + h + ")"), 
							pos);
					pos = check(l, line, 
							layout.getFieldFromName("daily-purch-val (" + i + ", " + w + ", " + d + ", " + h + ")"),
							pos);
				}
				pos = check(l, line, layout.getFieldFromName("d-purch-val (" + i + ", " + w + ", " + d + ")"), pos);
			}
			pos = check(l, line, layout.getFieldFromName("week-purch-val (" + i + ", " + w + ")"), pos);
		}
		pos = check(l, line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);
		pos = check(l, line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		return pos;
	}



	private int checkPurchases3(List<IFieldDetail> l, AbstractLine line, int week, int days, int hours, int pos,
			int i) {
		
		LayoutDetail layout = line.getLayout();
		pos += updateField(l, line, "week-of-month-2 (" + i + ")", week);
		pos += updateField(l, line, "days-2 (" + i + ")", days);
		pos += updateField(l, line, "hours-2 (" + i + ")", hours);

		for (int w = 0; w < week; w++) {
			int dd = (days + w) % 4;
			pos = check(l, line, layout.getFieldFromName("week-purch (" + i + ", " + w + ")"), pos);
			pos += updateField(l, line, "days-21 (" + i + ", " + w + ")", dd);
			for (int d = 0; d < dd; d++) {
				int hh = (hours + d + i) % 5;
				pos = check(l, line, layout.getFieldFromName("d-purch (" + i + ", " + w + ", " + d + ")"), pos);
				pos += updateField(l, line, "hours-21 (" + i + ", " + w + ", " + d + ")", hh);
				
				for (int h = 0; h < hh; h++) {
					pos = check(l, line, 
							layout.getFieldFromName("daily-purch (" + i + ", " + w + ", " + d + ", " + h + ")"), 
							pos);
					pos = check(l, line, 
							layout.getFieldFromName("daily-purch-val (" + i + ", " + w + ", " + d + ", " + h + ")"),
							pos);
				}
				pos = check(l, line, layout.getFieldFromName("d-purch-val (" + i + ", " + w + ", " + d + ")"), pos);
			}
			pos = check(l, line, layout.getFieldFromName("week-purch-val (" + i + ", " + w + ")"), pos);
		}
		pos = check(l, line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);
		pos = check(l, line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		return pos;
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
