/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;

import org.junit.Test;

/**
 * Check cobol2Xml when ther are redefines
 * 
 * @author Bruce Martin
 *
 */
public class TstRedefine {
	private static final String COPYBOOK
				= "        01 the-rec.\n"
				+ "           03 Base-Data PIC X(14).\n"
				+ "           03 Data REDEFINES Base-Data.\n"
				+ "              05 Val1      PIC S9(7) COMP-3.\n"
				+ "              05 Val2      PIC X(10).\n"
				+ "              05 redefines Val2.    \n"
				+ "                  07 Val3      PIC 9(5).\n"
				+ "                  07 Val4      PIC X(5).\n"
				;


	private static String XML_01
					= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><the-rec><Base-Data></Base-Data>"
					+ "<Data><Val1>#</Val1><Val2>$</Val2><Val3>#</Val3><Val4></Val4></Data></the-rec></CobolData>";
	private static String XML_03
					= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><the-rec><Base-Data></Base-Data>"
					+ "<Data><Val1>#</Val1><Val2></Val2><Val3></Val3><Val4>#</Val4></Data></the-rec></CobolData>";
	@Test
	public void test01() throws IOException, XMLStreamException {
		
		ICobol2Xml c2x = Cobol2Xml.newCobol2Xml(new StringReader(COPYBOOK), "the-rec")
								.setFont("");
		
		for (int i = 0; i < 15000; i++) {
			chk1(c2x, i);
		}
		for (int i = 80000; i < 82200; i++) {
			chk1(c2x, i);
		}
	}

	/**
	 * @param c2x
	 * @param i
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void chk1(ICobol2Xml c2x, int i) throws IOException,
			XMLStreamException {
		AbstractLine line = c2x.asIOBuilder().newLine();
		line.getFieldValue("Val1").set(i);
		line.getFieldValue("Val3").set(i);
		line.getFieldValue("Val4").set(" ");
		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ByteArrayOutputStream cs = new ByteArrayOutputStream();
		//OutputStream os = new FileOutputStream("g:\\Temp\\tt.xml");
		byte[] data = line.getData();
		
		c2x.cobol2xml(new ByteArrayInputStream(data), os);
		os.close();
		byte[] xmlBytes = os.toByteArray();
		
		String v = Integer.toString(i);
		
		
		c2x.xml2Cobol(new ByteArrayInputStream(xmlBytes), cs);
		
		StringBuilder b = Conversion.replace(XML_01, "#", v);
		b = Conversion.replace(b, "$", "00000".substring(v.length()) + v);

//			System.out.println();
//			System.out.println(new String(xmlBytes));
//			System.out.println(b.toString());

		assertEquals(b.toString(), new String(xmlBytes));
		assertArrayEquals(data, cs.toByteArray());
	}
	
	@Test
	public void test02() throws IOException, XMLStreamException {
		
		ICobol2Xml c2x = Cobol2Xml.newCobol2Xml(new StringReader(COPYBOOK), "the-rec")
								.setFont("");
		
		for (int i = 0; i < 15000; i++) {
			chk2(c2x, i);
		}
		
		for (int i = 80000; i < 82200; i++) {
			chk2(c2x, i);
		}
	}

	/**
	 * @param c2x
	 * @param i
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void chk2(ICobol2Xml c2x, int i) throws IOException,
			XMLStreamException {
		AbstractLine line = c2x.asIOBuilder().newLine();
		line.getFieldValue("Val1").set(i);
		line.getFieldValue("Val3").set(i);
		line.getFieldValue("Val4").setToLowValues();
		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ByteArrayOutputStream cs = new ByteArrayOutputStream();
		//OutputStream os = new FileOutputStream("g:\\Temp\\tt.xml");
		byte[] data = line.getData();
		
		c2x.cobol2xml(new ByteArrayInputStream(data), os);
		os.close();
		byte[] xmlBytes = os.toByteArray();
		
		String v = Integer.toString(i);
		
		
		c2x.xml2Cobol(new ByteArrayInputStream(xmlBytes), cs);
		
		StringBuilder b = Conversion.replace(XML_01, "#", v);
		b = Conversion.replace(b, "$", "00000".substring(v.length()) + v);

//			System.out.println();
//			System.out.println(new String(xmlBytes));
//			System.out.println(b.toString());

		assertEquals(b.toString(), new String(xmlBytes));
		byte[] newData = cs.toByteArray();
		assertEquals(data.length, newData.length);
		
		for (int j = 0; j < data.length; j++) {
			if (data[j] == newData[j] || (data[j] == 0 && newData[j] == 32)) {
				
			} else {
				assertEquals(i + ", " + j, data[j], newData[j]);
			}
		}
		//assertArrayEquals(data, newData);
	}

	@Test
	public void test03() throws IOException, XMLStreamException {
		
		ICobol2Xml c2x = Cobol2Xml.newCobol2Xml(new StringReader(COPYBOOK), "the-rec")
								.setFont("");
		
		for (int i = 0; i < 15000; i++) {
			chk3(c2x, i);
		}
		
		for (int i = 80000; i < 82200; i++) {
			chk3(c2x, i);
		}

	}

	/**
	 * @param c2x
	 * @param i
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void chk3(ICobol2Xml c2x, int i) throws IOException,
			XMLStreamException {
		AbstractLine line = c2x.asIOBuilder().newLine();
		line.getFieldValue("Val1").set(i);
		line.getFieldValue("Val4").set(i);
		line.getFieldValue("Val3").setToLowValues();
		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ByteArrayOutputStream cs = new ByteArrayOutputStream();
		//OutputStream os = new FileOutputStream("g:\\Temp\\tt.xml");
		byte[] data = line.getData();
		
		c2x.cobol2xml(new ByteArrayInputStream(data), os);
		os.close();
		byte[] xmlBytes = os.toByteArray();
		
		String v = Integer.toString(i);
		
		
		c2x.xml2Cobol(new ByteArrayInputStream(xmlBytes), cs);
		
		StringBuilder b = Conversion.replace(XML_03, "#", v);
//			b = Conversion.replace(b, "$", "00000".substring(v.length()) + v);

//			System.out.println();
//			System.out.println(new String(xmlBytes));
//			System.out.println(b.toString());

		assertEquals(b.toString(), new String(xmlBytes));
		byte[] newData = cs.toByteArray();
		assertEquals(data.length, newData.length);
		
		for (int j = 0; j < data.length; j++) {
			if (data[j] == newData[j] || (data[j] == 0 && newData[j] == 48)) {
				
			} else {
				assertEquals(i + ", " + j, data[j], newData[j]);
			}
		}
		//assertArrayEquals(data, newData);
	}

}
