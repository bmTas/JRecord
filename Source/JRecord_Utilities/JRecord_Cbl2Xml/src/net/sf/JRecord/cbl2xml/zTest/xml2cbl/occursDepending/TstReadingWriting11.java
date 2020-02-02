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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException; 
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.TstXmlConstants;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstReadingWriting11 extends TestCase {
	
	public void test11()  throws IOException, RecordException, XMLStreamException {
		tst("ArrayDep11.cbl", "11", Constants.IO_STANDARD_TEXT_FILE, false);
	}
	
	
	public void test12()  throws IOException, RecordException, XMLStreamException {
		tst("ArrayDep12.cbl", "12", Constants.IO_STANDARD_TEXT_FILE, false);
	}
	
	
	
	private void tst(String copybook, String id, int fileOrg, boolean sep) throws IOException, RecordException, XMLStreamException {
		String copybookFileName = TstXmlConstants.COBOL_DIRECTORY + copybook;
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(fileOrg);
		StringBuilder b = new StringBuilder();
		AbstractLine lastLine = null;
//		for (int i = 0; i <= 5; i++) {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		AbstractLineWriter w = ioBuilder.newWriter(out);
		try {
			for (int i = 0; i <= 5; i++) {
				for (int j = 0; j <= 5; j++) {
					lastLine = generateLine(ioBuilder.newLine(), i, j, sep);
					w.write(lastLine);
				}
			}
		} finally {
			w.close();
		}
			
		byte[] byteArray = out.toByteArray();
		b.append(new String(byteArray)).append('\n');
		//System.out.println(new String(byteArray));
			
		ICobol2Xml cbl2xml = Cobol2GroupXml.newCobol2Xml(copybookFileName)
			      .setFileOrganization(fileOrg)
			      .setDialect(ICopybookDialects.FMT_MAINFRAME);
				

		String xmlFileName = TstXmlConstants.XML_DIRECTORY + "XmlOut" + id + ".xml";
		byte[] bytes = out.toByteArray();
		//FileOutputStream xmlOut = new FileOutputStream("G:/Temp/JRecord/XmlOut" + id + ".xml");
		ByteArrayOutputStream xmlOut = new ByteArrayOutputStream();
		cbl2xml.cobol2xml(new ByteArrayInputStream(bytes), 
				xmlOut);
		xmlOut.close();
		byte[] xmlByteArray = xmlOut.toByteArray();
		Cb2XmlCode.compare("XmlCompare " +id, xmlFileName, xmlByteArray);
		
		ByteArrayOutputStream out2 = new ByteArrayOutputStream();
		ByteArrayInputStream xmlInStream = new ByteArrayInputStream(xmlByteArray);
		cbl2xml.xml2Cobol(xmlInStream, out2);
		xmlInStream.close();
		System.out.println();
		System.out.println();
			
		check(ioBuilder.newReader(new ByteArrayInputStream(byteArray)));
		check(ioBuilder.newReader(new ByteArrayInputStream(out2.toByteArray())));
	}




	/**
	 * @param r
	 * @throws RecordException
	 * @throws IOException
	 */
	public void check(AbstractLineReader r) throws RecordException, IOException {
		try {
			for (int i = 0; i <= 5; i++) {
				for (int j = 0; j <= 5; j++) {
					checkLine(r.read(), i, j);
				}
			}
		} finally {
				r.close();
		}
	}
	
	private AbstractLine generateLine(AbstractLine line, int levelCount, int attrCount, boolean sep) throws RecordException {
		line.getFieldValue("Record-Type").set("r");
		line.getFieldValue("Level-Count").set(levelCount);
		line.getFieldValue("Attr-Count").set(attrCount);
		
		for (int i = 0; i < levelCount; i++) {
			line.getFieldValue("Level (" + i + ")").set(i * 100 + attrCount);
			for (int j = 0; j < attrCount; j++) {
				line.getFieldValue("Attr (" + i + ", " + j + ")").set(i * 10 + j);
			}
		}
		return line;
	}

	private void checkLine(AbstractLine line, int levelCount, int attrCount) throws RecordException {

		assertEquals("r", line.getFieldValue("Record-Type").asString());
		assertEquals(levelCount, line.getFieldValue("Level-Count").asInt());
		assertEquals(attrCount, line.getFieldValue("Attr-Count").asInt());
		
		
		String pref = levelCount + ", " + attrCount + " : ";
		
		for (int i = 0; i < levelCount; i++) {
			assertEquals(pref + i, i * 100 + attrCount, line.getFieldValue("Level (" + i + ")").asInt());
			for (int j = 0; j < attrCount; j++) {
				assertEquals(pref + i + ", " + j, i * 10 + j, line.getFieldValue("Attr (" + i + ", " + j + ")").asInt());
			}
		}

	}

}
