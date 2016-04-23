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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl.cobol;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CreateArrayFile {

	public CreateArrayFile() throws IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
						.newIOBuilder(Cb2XmlCode.getFullName("cobol/ArrayCopybook.cbl"))
								.setDialect(ICopybookDialects.FMT_FUJITSU)
								.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
						;
		BufferedWriter w = new BufferedWriter( new FileWriter("G:\\temp\\ArrayFile.txt"));
		
		for (int i = 0; i < 20; i++) {
			AbstractLine l = ioBldr.newLine();
			long v1 = ((long) (i)) * 100000000;
			l.getFieldValue( "field-1" ).set(v1 + 1);
			l.getFieldValue( "field-2" ).set(v1 + 2);
			for (int j = 0; j < 7; j++) {
				long v2 = v1 + j * 1000000;
				l.getFieldValue( "field-4 (" + j + ")" ).set((long)v2 + 4);
				l.getFieldValue( "field-5 (" + j + ")").set((long) (v2 + 5));
				for (int k = 0; k < 6; k++) {
					long v3 = v2 + k * 10000;
					l.getFieldValue( "field-7 (" + j + ", " + k + ")" ).set(v3 + 7);
					for (int i1 = 0; i1 < 5; i1++) {
						long v4 = v3 + i1 * 100;
						l.getFieldValue( "field-a (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 8);
						l.getFieldValue( "field-b (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 9);
						l.getFieldValue( "field-c (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 3);
					}
				}
			}
			w.write(l.getFullLine());
			w.newLine();
		}
		w.close();
		
	}
	
	public static void main(String[] args) throws IOException {
		new CreateArrayFile();
	}
}
