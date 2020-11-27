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
public class TstGroupNames1 extends TestCase {

	private static String cobolCopybook
			= "      01 COMPANY-RECORD.\n"
			+ "         05 COMPANY-NAME     PIC X(30).\n"
			+ "         05 EMPLOYEE-LIST.\n"
			+ "            10 PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 VICE-PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 OTHERS.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n";


	private static String dataFile
				= "BM                            Martin         Bruce   XX             YY      Mr        Aa             Bb      Me             George Town    Ta07253\n"
				+ "JPY                           John           Young   Young          George  Mr        Hary           Vander  123            456            1100111\n"
				+ "Fleetwood Mac                 Fleetwood      Mick    Stevie         Nicks   Ms        McVie          Christinx              y              z 01234\n"
				;

	private static String[][] expected = {
		{"BM", "Bruce", "Martin", "YY", "XX", "Bb", "Aa"},
		{"JPY", "Young", "John", "George", "Young", "Vander", "Hary"},
		{"Fleetwood Mac", "Mick", "Fleetwood", "Nicks", "Stevie", "Christin", "McVie"},
	};
	
	public void test1() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_INTEL)
					.setFileOrganization(Constants.IO_BIN_TEXT);
		
		tst(ioBldr);
	}
	
	public void test2() throws Exception {
		ICobolIOBuilder ioBldr1 = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_INTEL)
					.setFileOrganization(Constants.IO_BIN_TEXT);
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		
		(new RecordEditorXmlWriter()).writeCopyBook(os, ioBldr1.getExternalRecord(), new TextLog());
		
		byte[] byteArray = os.toByteArray();
		System.out.println(new String(byteArray));
		IIOBuilder ioBldr2 = JRecordInterface1.SCHEMA_XML.newIOBuilder(new ByteArrayInputStream(byteArray), "COMPANY-RECORD");
		tst(ioBldr2);
	}

	
	public void test3() throws Exception {
		ICobolIOBuilder ioBldr1 = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(cobolCopybook.getBytes()), "COMPANY-RECORD")
					.setDialect(ICopybookDialects.FMT_INTEL)
					.setFileOrganization(Constants.IO_BIN_TEXT);
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		
		(new RecordEditorXmlWriter()).writeCopyBook(os, ioBldr1.getExternalRecord(), new TextLog());
		
		String s = os.toString();
		System.out.println(s);
		
		try {
		CompareXmlCode.compare(
				"Compare Xml: ", 
				TstGroupNames1.class.getResource("GroupNamesCopybook1.xml").getFile(),
				CompareXmlCode.stringToDom(s));
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}

	private void tst(IIOBuilder ioBldr) throws RecordException, IOException {
		LayoutDetail schema = ioBldr.getLayout();

		int recordIdx = 0;  // since there is only one record type, the record index must be zero
		                    // If there where more than one record, you could use:
		                    //    schema.getRecordIndex("COMPANY-RECORD");

			// Retrieve the Record-Definition
		RecordDetail record = schema.getRecord(recordIdx);

			// ** Retrieve the Field definitions from the RecordDefinition **
			// ** -------------------------------------------------------- **
		IFieldDetail presidentFirstNameFld     = record.getGroupField("PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld         = record.getGroupField("OTHERS", "FIRST-NAME");

		IFieldDetail presidentLastNameFld      = record.getGroupField("PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentLastNameFld  = record.getGroupField("VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherLastNameFld          = record.getGroupField("OTHERS", "LAST-NAME");


			//Get the compane-name field definition.
		IFieldDetail companyNameFld = schema.getFieldFromName("COMPANY-NAME");

		AbstractLineReader reader  = ioBldr.newReader(new ByteArrayInputStream(dataFile.getBytes())) ;
		AbstractLine line;


		int i = 0;
		while ((line = reader.read()) != null) {
			int j = 0; 
			assertEquals(expected[i][j++], line.getFieldValue(companyNameFld)           .asString() );
			assertEquals(expected[i][j++], line.getFieldValue(presidentFirstNameFld)    .asString() );
			assertEquals(expected[i][j++], line.getFieldValue(presidentLastNameFld)     .asString() );
			assertEquals(expected[i][j++], line.getFieldValue(vicePresidentFirstNameFld).asString() );
			assertEquals(expected[i][j++], line.getFieldValue(vicePresidentLastNameFld) .asString() );
			assertEquals(expected[i][j++], line.getFieldValue(otherFirstNameFld)        .asString() );
			assertEquals(expected[i][j++], line.getFieldValue(otherLastNameFld)         .asString() );
			i += 1;
		}

		reader.close();
	}
}
