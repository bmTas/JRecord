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

package net.sf.JRecord.zTest.GroupNames;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.base.RecordEditorXmlWriter;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

/**
 * This program test accessing Duplicate field names 
 * and importing / exporting group name fields
 *
 * @author Bruce Martin
 *
 */
public class TstGroupNames2 extends TestCase {

	private static String cobolCopybook
					= "        01  xx.\n"
					+ "            03 A.\n"
					+ "               05 B.\n"
					+ "                  07 Field-1    pic x(3).\n"
					+ "               05 C.\n"
					+ "                  07 Field-1    pic 9(4).\n"
					+ "            03 B.\n"
					+ "               05 A.\n"
					+ "                  07 Field-1    pic s9(6).\n"
					+ "            03 C.\n"
					+ "               05 A.\n"
					+ "                  07 Field-1    pic s9(3)v999.\n"
					+ "               05 D.\n"
					+ "                  07 Field-1    pic s9(5)v999.\n"
			;


	private static String dataFile
				= "11 001200001C01400{0001500{\n"
				+ "21 002200002C02400{0002500{\n"
				+ "31 003200003C03400{0003500{\n"
				+ "41 004200004C04400{0004500{\n"
				;

	private static String[][] expected = {
		{"11", "13", "14.000"},
		{"21", "23", "24.000"},
		{"31", "33", "34.000"},
		{"41", "43", "44.000"},
	};
	
	
	public void test1() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_BIN_TEXT);

		tst1(ioBldr);
		tst2(ioBldr);
	}
	
	public void test2() throws Exception {
		ICobolIOBuilder ioBldr1 = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_BIN_TEXT);
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		
		(new RecordEditorXmlWriter()).writeCopyBook(os, ioBldr1.getExternalRecord(), new TextLog());
		
		IIOBuilder ioBldr2 = JRecordInterface1.SCHEMA_XML.newIOBuilder(new ByteArrayInputStream(os.toByteArray()), "COMPANY-RECORD");
		tst1(ioBldr2);
		tst2(ioBldr2);
	}

	
	public void test3() throws Exception {
		ICobolIOBuilder ioBldr1 = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_BIN_TEXT);
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		
		(new RecordEditorXmlWriter()).writeCopyBook(os, ioBldr1.getExternalRecord(), new TextLog());
		
		String s = os.toString();
		System.out.println(s);
		
		try {
			CompareXmlCode.compare(
				"Compare Xml: ", 
				this.getClass().getResource("GroupNamesCopybook2.xml").getFile(),
				CompareXmlCode.stringToDom(s));
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}


	@SuppressWarnings("deprecation")
	private void tst1(IIOBuilder ioBldr) throws RecordException, IOException {
		LayoutDetail schema = ioBldr.getLayout();
		int recordIdx = 0;  // since there is only one record type, the record index must be zero
		                    // If there where more than one record, you could use:
		                    //    schema.getRecordIndex("COMPANY-RECORD");

			// Retrieve the Record-Definition
		RecordDetail record = schema.getRecord(recordIdx);

			// ** Retrieve the Field definitions from the RecordDefinition **
			// ** -------------------------------------------------------- **
		IFieldDetail abField = record.getUniqueFieldGroupsInSequence("Field-1", "A", "B");
		IFieldDetail baField = record.getUniqueFieldGroupsInSequence("Field-1", "B", "A");
		IFieldDetail caField = record.getUniqueFieldGroupsInSequence("Field-1", "C", "A");

			// Retrieve the File-Reader Constants.IO_BIN_TEXT does byte level read lines
			// i.e. it retrieves lines (of bytes) from the file.
		tstReader( ioBldr.newReader(
				new ByteArrayInputStream(dataFile.getBytes())),
				abField, baField, caField);
		
	}
	
	private void tst2(IIOBuilder ioBldr) throws RecordException, IOException {
		LayoutDetail schema = ioBldr.getLayout();
		int recordIdx = 0;  // since there is only one record type, the record index must be zero
		                    // If there where more than one record, you could use:
		                    //    schema.getRecordIndex("COMPANY-RECORD");

			// Retrieve the Record-Definition
		RecordDetail record = schema.getRecord(recordIdx);

			// ** Retrieve the Field definitions from the RecordDefinition **
			// ** -------------------------------------------------------- **
		IFieldDetail abField = record.getGroupField("A", "B", "Field-1");
		IFieldDetail baField = record.getGroupField("B", "A", "Field-1");
		IFieldDetail caField = record.getGroupField("C", "A", "Field-1");

			// Retrieve the File-Reader Constants.IO_BIN_TEXT does byte level read lines
			// i.e. it retrieves lines (of bytes) from the file.
		tstReader( ioBldr.newReader(
				new ByteArrayInputStream(dataFile.getBytes())),
				abField, baField, caField);
		
	}

	
	private void tstReader(AbstractLineReader reader, IFieldDetail abField, IFieldDetail baField, IFieldDetail caField) throws IOException {
		AbstractLine line;

		int i = 0;
		while ((line = reader.read()) != null) {
			int j = 0;
			assertEquals(expected[i][j++], line.getFieldValue(abField) .asString());
			assertEquals(expected[i][j++], line.getFieldValue(baField) .asString());
			assertEquals(expected[i][j++], line.getFieldValue(caField) .asString());
			
			i += 1;
		}

		reader.close();
	}
}
