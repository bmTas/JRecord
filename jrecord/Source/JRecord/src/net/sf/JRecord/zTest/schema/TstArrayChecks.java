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

package net.sf.JRecord.zTest.schema;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.w3c.dom.Document;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.jaxb.Condition;
import net.sf.JRecord.schema.jaxb.Copybook;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import junit.framework.TestCase;


/**
 * Test the Array-Checks
 * 
 * @author Bruce Martin
 *
 */
public class TstArrayChecks extends TestCase {

	public static final String COBOL_COPYBOOK
					= "       01  ArrayCheckData.\n"
					+ "           05  Item-Count             pic 99.\n"
					+ "           05  Item                   pic xx occurs 8.\n"
					+ "           05  After-Fld              pic x(3).\n";

	private static final Item ITEM = getArrayItem();
	
	public void testStopSpaces01() throws IOException {
		
		IArrayItemCheck stopAtSpaces = ArrayElementChecks.INSTANCE.newStopAtSpaces();
		int stop = IArrayItemCheck.R_STOP;
		tstSpaces(stopAtSpaces, stop);
	}
	
	
	public void testSkipSpaces01() throws IOException {
		
		IArrayItemCheck stopAtSpaces = ArrayElementChecks.INSTANCE.newSkipSpaces();
		int skip = IArrayItemCheck.R_SKIP;
		tstSpaces(stopAtSpaces, skip);
	}


	/**
	 * @param stopAtSpaces
	 * @param skip
	 * @throws IOException
	 */
	private void tstSpaces(IArrayItemCheck stopAtSpaces, int skip)
			throws IOException {
		doTst(
				new TstDataString(true, "11", "  "),
				stopAtSpaces,
				skip
				);
		doTst(
				new TstDataString(true, " 0", "  "),
				stopAtSpaces,
				skip
				);
		doTst(
				new TstDataString(true, "0 ", "  "),
				stopAtSpaces,
				skip
				);
		doTst(
				new TstDataString(true, "1 ", "  "),
				stopAtSpaces,
				skip
				);
		doTst(
				new TstDataString(false, "11", "  "),
				stopAtSpaces,
				skip
				);
	}
	
	
	public void testStopSpacesZero01() throws IOException {
		IArrayItemCheck stopAtSpacesZeros = ArrayElementChecks.INSTANCE.newStopAtSpacesZeros();
		int stop = IArrayItemCheck.R_STOP;
		tstSacesZeros(stopAtSpacesZeros, stop);
	}

	
	public void testSkipSpacesZero01() throws IOException {
		IArrayItemCheck stopAtSpacesZeros = ArrayElementChecks.INSTANCE.newSkipSpacesZeros();
		int stop = IArrayItemCheck.R_SKIP;
		tstSacesZeros(stopAtSpacesZeros, stop);
	}


	/**
	 * @param stopAtSpacesZeros
	 * @param stop
	 * @throws IOException
	 */
	private void tstSacesZeros(IArrayItemCheck stopAtSpacesZeros, int stop)
			throws IOException {
		doTst(	new TstDataString(true, "11", "  "),
				stopAtSpacesZeros,
				stop
				);
		doTst(	new TstDataString(true, "11", "0 "),
				stopAtSpacesZeros,
				stop
				);
		doTst(	new TstDataString(true, "11", " 0"),
				stopAtSpacesZeros,
				stop
				);
		doTst(	new TstDataString(true, "11", "00"),
				stopAtSpacesZeros,
				stop
				);
		doTst(	new TstDataString(false, "11", " 0"),	
				stopAtSpacesZeros,
				stop
				);
	}
	
	public void testLowValues() throws IOException {
		doTst(	new TstDataByte("--", (byte) 0),	
				ArrayElementChecks.INSTANCE.newSkipLowValues(),
				IArrayItemCheck.R_SKIP
				);
		doTst(	new TstDataByte("--", (byte) 0),	
				ArrayElementChecks.INSTANCE.newStopAtLowValues(),
				IArrayItemCheck.R_STOP
				);
	}

	
	public void testHighValues() throws IOException {
		doTst(	new TstDataByte("--", (byte) -1),	
				ArrayElementChecks.INSTANCE.newSkipHighValues(),
				IArrayItemCheck.R_SKIP
				);
		doTst(	new TstDataByte("--", (byte) -1),	
				ArrayElementChecks.INSTANCE.newStopAtHighValues(),
				IArrayItemCheck.R_STOP
				);
	}

	
	public void testVarCheck() throws IOException {
		doTst(	new TstDataString(true, "-1", "*2"),	
				ArrayElementChecks.INSTANCE.newIndexCheck("Item-Count"),
				IArrayItemCheck.R_STOP,
				8
				);
		
		doTst(	new TstDataString(false, "-1", "*2"),	
				ArrayElementChecks.INSTANCE.newIndexCheck("Item-Count"),
				IArrayItemCheck.R_STOP,
				8
				);
	}

	private final int[] indexs = {};
	public void doTst(IGetData getData, IArrayItemCheck check, int expectedResult) {
		doTst(getData, check, expectedResult, 9);
	}

	public void doTst(IGetData getData, IArrayItemCheck check, int expectedResult, int testMax) {
		AbstractLine line;

		for (int i = 0; i < testMax; i++) {
			getData.setIndex(i);
			line = getData.getLine();
			tstForAllIndexs(check, expectedResult, line, i);
			assertEquals(i, line.getFieldValue("Item-Count").asInt());
			assertEquals("abc", line.getFieldValue("After-Fld").asString());
			
			if (expectedResult == IArrayItemCheck.R_STOP && i < 8) {
				getData.setIndex(9);
				check.updateForCount(line, ITEM, indexs, i);
				
				tstForAllIndexs(check, expectedResult, line, i);
			}
		}
	}


	/**
	 * @param check
	 * @param expectedResult
	 * @param line
	 * @param i
	 * @return
	 */
	private void tstForAllIndexs(IArrayItemCheck check, int expectedResult,
			AbstractLine line, int i) {
		int end = expectedResult == IArrayItemCheck.R_STOP 
				? i
				: 8;
		
		
		for (int j = 0; j <= end; j++) {
			int r = check.checkItem(line, ITEM, indexs, j);
			if (i == j && i < 8) {
				assertEquals(i + ", " + j, expectedResult, r);
			} else {
				assertEquals(i + ", " + j, IArrayItemCheck.R_PROCESS, r);
			}
		}
	}
	
	
	public static LayoutDetail getLayout() throws IOException {
		return JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COBOL_COPYBOOK), "ArrayCheckData")
						.getLayout();
	}
	
	private static Item getArrayItem() {
		try {
			Document doc = net.sf.JRecord.External.Def.Cb2Xml.convertToXMLDOM(
					new StringReader(COBOL_COPYBOOK), "ArrayCheckData", ICopybookDialects.FMT_MAINFRAME,
					false, Cb2xmlConstants.USE_STANDARD_COLUMNS);
	        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
	        
	        Unmarshaller unmarshaller = jc.createUnmarshaller();
	        JAXBElement<Copybook> jaxbCopybook = unmarshaller.unmarshal(doc, Copybook.class);
	        return  findItem(jaxbCopybook.getValue().getCobolItems());

		} catch (ParserException e) {
			e.printStackTrace();
		} catch (LexerException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (JAXBException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	
	private static Item findItem(List<Item> items) {
		for (Item itm : items) {
			if ("Item".equalsIgnoreCase(itm.getName())) {
				return itm;
			}
			List<Item> childItems = itm.getChildItems();
			if (childItems != null && childItems.size() > 0) {
				Item r = findItem(childItems);
				if (r != null) {
					return r;
				}
			}
		}
		return null;
	}
	
	public class TstDataString implements IGetData {
		final LayoutDetail layout;
		final AbstractLine line;
		final String nonBlankData, blankData;
		
		TstDataString(boolean charLine, String nonBlankData, String blankData) throws IOException {
			layout = getLayout();
			if (charLine) {
				line = new CharLine(layout, "");
			} else {
				line = new Line(layout);
			}
			
			this.nonBlankData = nonBlankData;
			this.blankData    = blankData;
		}

		/**
		 * @return the line
		 */
		public final AbstractLine getLine() {
			return line;
		}
		
		public void setIndex(int idx) {
			for (int i = 0; i < 8; i++) {
				line.getFieldValue("Item (" + i + ")").set(nonBlankData);
			}
			int id = Math.min(8, idx);
			line.getFieldValue("Item-Count").set(id);
			if (idx >= 0 && idx < 8) {
				line.getFieldValue("Item (" + (idx) + ")").set(blankData);
			}
			line.getFieldValue("After-Fld").set("abc");
		}
	}
	
	
	public class TstDataByte implements IGetData {
		final LayoutDetail layout;
		final AbstractLine line;
		final String nonBlankData;
		final byte blankData;
		
		TstDataByte(String nonBlankData, byte blankData) throws IOException {
			layout = getLayout();
			line = new Line(layout);
						
			this.nonBlankData = nonBlankData;
			this.blankData    = blankData;
		}

		/**
		 * @return the line
		 */
		public final AbstractLine getLine() {
			return line;
		}
		
		public void setIndex(int idx) {
			for (int i = 0; i < 8; i++) {
				line.getFieldValue("Item (" + i + ")").set(nonBlankData);
			}
			int id = Math.min(8, idx);
			byte[] data = line.getData();
			line.getFieldValue("Item-Count").set(id);
			if (idx >= 0 && idx < 8) {
				int pos = line.getFieldValue("Item (" + (idx) + ")").getFieldDetail().getPos();
				data[pos-1] = blankData;
				data[pos] = blankData;
			}
			line.getFieldValue("After-Fld").set("abc");
		}
	}

	private interface IGetData {
		AbstractLine getLine();
		void setIndex(int idx);
	}
}
