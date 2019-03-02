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

package net.sf.JRecord.zTest.io.ibm4680;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import junit.framework.TestCase;

/**
 * Run through and read the Point of Sale files
 * This will check there are no obvious errors in the
 * record read objects
 * 
 * @author Bruce Martin
 *
 */
public class TstReadWrite4680Price extends TestCase {
	
	private String[][] expectedLines = {
			{"2", "256312", "In Store Promotion", "406150030", "407120030", null, "0", "0"},
			{"8", "19775531", "BAD COMPANY E15440 DVD M", "3", "17.99", "0", "0", "0", "0"},
			{"8", "33896472", "CHICAGO DVD M E03860", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19775555", "COUNT OF MONTE CRISTO E15380 D", "3", "17.99", "0", "0", "0", "0"},
			{"8", "17976374", "DVD AIR FORCE ONE", "3", "17.99", "0", "0", "0", "0"},
			{"8", "17976718", "DVD GONE IN 60 SECONDS.", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19775500", "GI JANE E00880 DVD M", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19061504", "GOOD MORNING VIETNAM S/EDIT M1", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19429465", "KATE AND LEOPOLD DVD PG", "3", "17.99", "0", "0", "0", "0"},
			{"8", "18227970", "MEMENTO DVD MA", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19775593", "PRETTY WOMAN SE E10270 DVD M", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19775463", "REIGN OF FIRE E15490 DVD NYC", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19431802", "SERENDIPITY DVD PG", "3", "17.99", "0", "0", "0", "0"},
			{"8", "32449600", "SIGNS (VISTA) DVD E50690", "3", "17.99", "0", "0", "0", "0"},
			{"8", "19424651", "SORORITY BOYS E50620 DVD NYC", "3", "17.99", "0", "0", "0", "0"},
			{"8", "32614077", "TERMINAL VELOCITY DVD E73660", "3", "17.99", "0", "0", "0", "0"},
			{"8", "18900668", "WHILE YOU WERE SLEEPING PG DVD", "3", "17.99", "0", "0", "0", "0"},	
	};
	public void testPriceRead1() throws Exception {
		tstRead(this.getClass().getResource("Pos_Price_2.bin").getFile(), -1, 17);
	}

	
	public void testWrite1() throws Exception {
		tstWrite(this.getClass().getResource("Pos_Price_2.bin").getFile(), -1, 17);
	}
	
	public void testPriceRead2() throws Exception {
		tstRead(this.getClass().getResource("Pos_Price_2.bin").getFile(), Constants.IO_CONTINOUS_NO_LINE_MARKER, 17);
	}

	
	public void testWrite2() throws Exception {
		tstWrite(this.getClass().getResource("Pos_Price_2.bin").getFile(), Constants.IO_CONTINOUS_NO_LINE_MARKER, 17);
	}
	
	private void tstRead(String filename, int fileStructure, int expected) throws Exception {	
		tstRead(loadSchema(fileStructure), new FileInputStream(filename), expected);
	}
	
	private void tstRead(LayoutDetail schema, InputStream in, int expected) throws Exception {
		//= RecordEditorXmlLoader.getExternalRecord(schemaXml, "Schema").asLayoutDetail();
		AbstractLineReader lineReader = LineIOProvider.getInstance().getLineReader(schema);
		AbstractLine l;
		
		lineReader.open(in, schema);
		int correct = 0, diff = 0, unknown = 0, i = 0;
		
		System.out.print("Line Lengths: ");
		
		while (i < expectedLines.length && (l = lineReader.read()) != null) {
			int pref = l.getPreferredLayoutIdx();
			
			System.out.print(", " +l.getData().length);
			
			if (pref < 0) {
				unknown += 1;
			} else {
				if (l.getData().length == schema.getRecord(pref).getLength() ) {
					correct += 1;
				} else {
					diff += 1;
				}
				
				for (int j = 0; j < expectedLines[i].length ; j++) {
					if (expectedLines[i][j] != null) {
						assertEquals(i + ", " + j + " ~ " + l.getFullLine(), expectedLines[i][j], l.getFieldValue(pref, j).asString());
					}
				}
			}
			i += 1;
		}
		System.out.println();
		
		System.out.println(in + "\t" + unknown + "\t" + correct + "\t" + diff);
		lineReader.close();
		assertEquals(0, unknown);
		assertEquals(expected, correct);
		assertEquals(0, diff);
	}
	
	
	private void tstWrite(String filename, int fileStructure, int expected) throws Exception {
		//= RecordEditorXmlLoader.getExternalRecord(schemaXml, "Schema").asLayoutDetail();
		LayoutDetail schema = loadSchema(fileStructure);
		InputStream in = new FileInputStream(filename);
		LineIOProvider ioProvider = LineIOProvider.getInstance();
		AbstractLineReader lineReader = ioProvider.getLineReader(schema);
		AbstractLineWriter lineWriter = ioProvider.getLineWriter(schema);
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		AbstractLine l;
		ArrayList<byte[]> lines = new ArrayList<byte[]>();
		
		lineReader.open(in, schema);
		lineWriter.open(out);
		int i = 0;
		
		System.out.print("            : ");
		while (i < expectedLines.length && (l = lineReader.read()) != null) {
			System.out.print(", " +l.getData().length);
			lineWriter.write(l);
			lines.add(l.getData());
			i += 1;
		}
		System.out.println();
	
		lineReader.close();
		lineWriter.close();
		
		byte[] bytes = out.toByteArray();
		tstRead(schema, new ByteArrayInputStream(bytes), expected);
		
		int p = 0;
		for (i = 0; i < lines.size(); i++) {
			byte[] lineAsBytes = lines.get(i);
			for (int j = 0; j < lineAsBytes.length; j++) {
				assertEquals(i+", " + j + ", " + p, lineAsBytes[j], bytes[p++]);
			}
		}
	}

	
	private LayoutDetail loadSchema(int fileStructure) throws Exception {
		String schemaFile = this.getClass().getResource("Price.Xml").getFile();
		ExternalRecord schema = (new RecordEditorXmlLoader())
									.loadCopyBook(schemaFile, 0, 0, "", 0, 0, null);
		schema.setFontName(Conversion.DEFAULT_ASCII_CHARSET);
		if (fileStructure >= 0) {
			schema.setFileStructure(fileStructure);
		}
		
		return schema.asLayoutDetail();
	}

}
