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
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

/**
 * Test Occurs depending in the shared header in a 
 * Spli_Redefines copybook
 * 
 * @author Bruce Martin
 *
 */
public class TstOccursDepending41 extends TestCase {

	private static final String MONTHS = "months";
	private static final String WEEK_NO = "week-no";

	public void testPositionCalc1() throws Exception {
		try {
			tstPosition("OccursDependingOn41.cbl");
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}
	

	
	private  void tstPosition(String copybookFile)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybookFile).getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE);


		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 12; j++) {
				System.out.println("Line: " + i + ", " + j);
				tstLine(ioBuilder.newLine(), i, j, 0, 0);
			}
		}

	}
	

	private void tstLine(AbstractLine line, int level, int month, int attr, int xx) throws RecordException {
		LayoutDetail layout = line.getLayout();
		IFieldDetail levelFld = layout.getFieldFromName("Level-Count");
		IFieldDetail monthFld = layout.getFieldFromName(MONTHS);
		IFieldDetail attrFld = layout.getFieldFromName("Attr-Count");
		//IFieldDetail purchCountFld = layout.getFieldFromName("total-purchase-count");
		
			/** Setting the Occurs Depending fields !!! **/
		line.getFieldValue(levelFld).set(level);
		line.getFieldValue(attrFld).set(attr);
		line.getFieldValue(monthFld).set(month);
		
		@SuppressWarnings("deprecation")
		int pos = attrFld.getEnd() + 1;
				
		for (int i = 0; i < level; i++) {
			pos = check(line, layout.getFieldFromName("level (" + i + ")"), pos);
		}

		pos += monthFld.getLen();
		
		pos = check(line, layout.getFieldFromName("week-of-month"), pos);
		pos = check(line, layout.getFieldFromName("days"), pos);
		for (int i = 0; i < month; i++) {
			//System.out.println("==> " + purchaseCount + ", sc=" + salesCount + ", week=" + week + " " + i);
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");

			pos = check(line, countFld, pos);
			pos = check(line, valueFld, pos);
		}

		pos = check(line, layout.getFieldFromName("total-sales"), pos);
		pos = check(line, layout.getFieldFromName(WEEK_NO), pos);		
	
	}

//	private void check(AbstractLine line, IFieldDetail fld) throws RecordException {
//		check(line, fld, fld.getPos());
//	}
	
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
