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

package net.sf.JRecord.cbl2xml.zTest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;

public class ChkRedefine {
	private static final String COPYBOOK
			= "        01 the-rec.\n"
			+ "           03 Base-Data PIC X(14).\n"
			+ "           03 Data REDEFINES Base-Data.\n"
			+ "                05 Val1      PIC S9(7) COMP-3.\n"
			+ "                05 Val2      PIC X(10).\n";
	
	public static void main(String[] args) throws IOException, XMLStreamException {
		ICobol2Xml c2x = Cobol2Xml.newCobol2Xml(new StringReader(COPYBOOK), "the-rec")
							.setFont("");
		
		AbstractLine line = c2x.asIOBuilder().newLine();
		char[] d = {
				//'a', 'B', 'C', 'D', 'E',
				(char) 0x41, (char) 0x42, (char) 0x43,(char) 0x44,(char) 0x45,
				(char) 0, (char) 0, (char) 0, (char) 0, (char) 0};
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ByteArrayOutputStream cs = new ByteArrayOutputStream();
		
		OutputStream fs = os; //new FileOutputStream("g:\\Temp\\t.xml");
		line.getFieldValue("Val1").set(8984993);
		line.getFieldValue("Val2").set(new String(d));
		
		byte[] data = line.getData();
		for (int i =0; i < data.length; i++) {
			System.out.print(" " + (0xFF & data[i]));
		}
		System.out.println();
		System.out.println(((char) 45) + " " + (0x89) + " " + line.getFullLine());
		
		c2x.cobol2xml(new ByteArrayInputStream(line.getData()), fs);
		fs.close();
		
		c2x.xml2Cobol(new ByteArrayInputStream(os.toByteArray()), cs);
		cs.close();
	}
}
