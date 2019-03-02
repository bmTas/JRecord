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

package net.sf.JRecord.zTest.External;


import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;

import junit.framework.TestCase;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorCsvLoader;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.CopybookWriter;
import net.sf.JRecord.External.base.RecordEditorCSVWriter;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.CommonCodeFields;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants; 

/**
 * Testing RecordEditorCSVReader/Writer classes
 * 
 * @author Bruce Martin
 *
 */
public class TstRecordEditCsvParser extends TestCase {

//	private final String csvDdirectory = "/home/bm/Work/RecordEditor/CsvCopybooks/";
//	private final String csvDirectory1 = "/home/bm/Work/RecordEditor/CsvCopybooks/Output/";

	
	private final String       poFileName = TstConstants.CSV_DIRECTORY + "poDtl.Txt";
	private final String poHeaderFileName = TstConstants.CSV_DIRECTORY + "poDtl_Header.Txt";
	private final String    poSkuFileName = TstConstants.CSV_DIRECTORY + "poDtl_Sku.Txt";

	private final String       poFileNameO= TstConstants.CSV_DIRECTORY_OUTPUT + "poDtl.Txt";

	
	private String eol    = "\n";
	private String [] poDetailLines = {
			"Record	0	9	<Tab>	0		Y	Ams PO Record",
			"SR	poDtl_Header	Record Type	H1	-1	N",
			"SR	poDtl_Sku	Record Type	D1	0	N"
	};
	private String [] poDetailHeaderLines = {
			"Record	0	1	<Tab>	0		N	ams PO Download: Header",
			"1	2	Record Type		0	0	0	",
			"3	5	Sequence Number		8	3	0	",
			"8	10	Vendor		7	0	0	",
			"18	12	PO		8	0	0	",
			"30	6	Entry Date	Format YYMMDD	0	0	0	",
			"36	8	Filler		0	0	0	",
			"44	2	beg01 code		0	0	0	",
			"46	2	beg02 code		0	0	0	",
			"48	4	Department		0	0	0	",
			"52	6	Expected Reciept Date	Format YYMMDD	0	0	0	",
			"58	6	Cancel by date	Format YYMMDD	0	0	0	",
			"68	1	EDI Type		0	0	0	",
			"69	6	Add Date	Format YYMMDD	0	0	0	",
			"75	1	Filler		0	0	0	",
			"76	10	Department Name		0	0	0	",
			"86	1	Prcoess Type	C/N Conveyable/Non-Conveyable	0	0	0	",
			"87	2	Order Type		0	0	0	",
	};
	private String [] poDetailSkuLines = {
			"Record	0	1	<Tab>	0		N	ams PO Download: Detail",
			"1	2	Record Type		0	0	0	",
			"3	9	Pack Qty		8	4	0	",
			"12	13	Pack Cost		8	4	0	",
			"25	13	APN		7	0	0	",
			"38	1	Filler		0	0	0	",
			"39	8	Product		7	0	0	",
			"72	15	pmg dtl tech key		0	0	0	",
			"87	15	Case Pack id		0	0	0	",
			"101	50	Product Name		0	0	0	",
	};

	private FieldDetail[] SKU_FIELDS = {
			CommonCodeFields.createField("Record Type", 1, 2, 0, Type.ftChar),
			CommonCodeFields.createField("Pack Qty", 3, 9, 4, Type.ftAssumedDecimal),
			CommonCodeFields.createField("Pack Cost", 12, 13, 4, Type.ftAssumedDecimal),
			CommonCodeFields.createField("APN", 25, 13, 0, Type.ftNumZeroPadded),
			CommonCodeFields.createField("Filler", 38, 1, 0, Type.ftChar),
			CommonCodeFields.createField("Product", 39, 8, 0, Type.ftNumZeroPadded),
			CommonCodeFields.createField("pmg dtl tech key", 72, 15, 0, Type.ftChar),
			CommonCodeFields.createField("Case Pack id", 87, 15, 0, Type.ftChar),
			CommonCodeFields.createField("Product Name", 101, 50, 0, Type.ftChar),
	};
	
	public void testLoadCopyBook1() throws Exception {
		
		System.out.println("Test 1");
		AbsSSLogger log = new TextLog();
		//IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		RecordEditorCsvLoader.Tab l = new RecordEditorCsvLoader.Tab();
		ExternalRecord copybook = l.loadCopyBook(new StringReader(concatenate(poDetailSkuLines)), "poDtl_Sku.Txt", 
				0, 0, "", CommonBits.getDefaultCobolTextFormat(),  0, 0, log);
		
		checkSkuCopybook(copybook);
		
		CommonCodeFields.checkFields("Po_Dtl_Sku", "", SKU_FIELDS, copybook.asLayoutDetail().getRecord(0));
	}
	
	public void testLoadCopyBookVariations() throws Exception {
		String[][] SkuLines = {
			{
					"Record	0	1	<Tab>	0		N	ams PO Download: Detail",
					"	2	Record Type		0	0	0	",
					"	9	Pack Qty		8	4	0	",
					"	13	Pack Cost		8	4	0	",
					"	13	APN		7	0	0	",
					"	1	Filler		0	0	0	",
					"	8	Product		7	0	0	",
					"72	15	pmg dtl tech key		0	0	0	",
					"	15	Case Pack id		0	0	0	",
					"101	50	Product Name		0	0	0	",
			},
			{
				"Record	0	1	<Tab>	0		N	ams PO Download: Detail",
				"1		Record Type		0	0	0	",
				"3		Pack Qty		8	4	0	",
				"12		Pack Cost		8	4	0	",
				"25		APN		7	0	0	",
				"38		Filler		0	0	0	",
				"39	8	Product		7	0	0	",
				"72		pmg dtl tech key		0	0	0	",
				"87	15	Case Pack id		0	0	0	",
				"101	50	Product Name		0	0	0	",
			},
			{
				"Record	0	1	<Tab>	0		N	ams PO Download: Detail",
				"		Record Type		0	0	0	",
				"3	9	Pack Qty		8	4	0	",
				"		Pack Cost		8	4	0	",
				"25	13	APN		7	0	0	",
				"		Filler		0	0	0	",
				"39	8	Product		7	0	0	",
				"72		pmg dtl tech key		0	0	0	",
				"87	15	Case Pack id		0	0	0	",
				"101	50	Product Name		0	0	0	",
			},
			{
				"Record	0	1	<Tab>	0		N	ams PO Download: Detail",
				"1	2	Record Type		0	0	0	",
				"		Pack Qty		8	4	0	",
				"12	13	Pack Cost		8	4	0	",
				"		APN		7	0	0	",
				"38	1	Filler		0	0	0	",
				"	8	Product		7	0	0	",
				"72	15	pmg dtl tech key		0	0	0	",
				"	15	Case Pack id		0	0	0	",
				"101	50	Product Name		0	0	0	",
			}
		};
		
		System.out.println("Test Variations");
		AbsSSLogger log = new TextLog();
		//IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		RecordEditorCsvLoader.Tab l = new RecordEditorCsvLoader.Tab();
		int i = 0;
		
		for (String[] lines : SkuLines) {
			ExternalRecord copybook = l.loadCopyBook(new StringReader(concatenate(lines)), "poDtl_Sku.Txt", 
					0, 0, "", CommonBits.getDefaultCobolTextFormat(),  0, 0, log);

			CommonCodeFields.checkFields("Po_Dtl_Sku: " + (i++), "", SKU_FIELDS, copybook.asLayoutDetail().getRecord(0));
		}

	}
	
	private String concatenate(String[] lines) {
		String sep = "";
		StringBuilder b = new StringBuilder();
		
		for (String s: lines) {
			b.append(sep).append(s);
			sep = "\n";
		}
		
		return b.toString();
	}
	
	
	public void testLoadCopyBook2() throws Exception {
	
		System.out.println("Test 2");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		IO.writeAFile(poHeaderFileName, poDetailHeaderLines, eol);
		IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		RecordEditorCsvLoader.Tab l = new RecordEditorCsvLoader.Tab();
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);
		
		checkCopybook(copybook);
		checkSkuCopybook(copybook.getRecord(1));
	}
	
	public void testLoadCopyBook3() throws Exception {
		
		System.out.println("Test 3");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		IO.writeAFile(poHeaderFileName, poDetailHeaderLines, eol);
		IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		CopybookLoader l = new RecordEditorCsvLoader.Tab(); 
		CopybookLoader l1= new RecordEditorCsvLoader.Comma();
		CopybookWriter w = new RecordEditorCSVWriter(",");
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);
		
		w.writeCopyBook(TstConstants.CSV_DIRECTORY_OUTPUT, copybook, log);
		ExternalRecord copybook1 = l1.loadCopyBook(poFileNameO, 0, 0, "", 0, 0, log);
		
		checkCopybook(copybook1);
		checkSkuCopybook(copybook1.getRecord(1));
	}

	public void testLoadCopyBook4() throws Exception {
		
		System.out.println("Test 4");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		IO.writeAFile(poHeaderFileName, poDetailHeaderLines, eol);
		IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		CopybookLoader l = new RecordEditorCsvLoader.Tab();

		CopybookWriter w = new RecordEditorCSVWriter("\t");
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);
		
		w.writeCopyBook(TstConstants.CSV_DIRECTORY_OUTPUT, copybook, log);

		compareFile2Array(poDetailLines, poFileNameO);
		compareFile2Array(poDetailHeaderLines, TstConstants.CSV_DIRECTORY_OUTPUT + "poDtl_Header.Txt");
		compareFile2Array(poDetailSkuLines, TstConstants.CSV_DIRECTORY_OUTPUT + "poDtl_Sku.Txt");
	}


	
	private void checkSkuCopybook(ExternalRecord copybook ) {
		assertEquals("1.1 Check no Sub records", 0, copybook.getNumberOfRecords());
		assertEquals("1.2 Check Fields = 9; actual=" + copybook.getNumberOfRecordFields(),9, copybook.getNumberOfRecordFields());
		assertEquals("1.3 Check File Structure=0; actual=" + copybook.getFileStructure(), 0, copybook.getFileStructure());
		assertEquals("1.4 Check Record Type=1; actual=" + copybook.getRecordType(), 1, copybook.getRecordType());
		assertEquals("1.5 Check Sep=<Tab>; actual=" + copybook.getDelimiter(), "<Tab>", copybook.getDelimiter());
		assertEquals("1.6 Check Style=0; actual=" + copybook.getRecordStyle(), 0, copybook.getRecordStyle());
		assertEquals("1.7 Check List=N; actual=" + copybook.getListChar(), "N", copybook.getListChar());
		
		ExternalField f = copybook.getRecordField(1);
		
		assertEquals("1.8 Check Field Name = 'Pack Qty'; actual=" + f.getName(), "Pack Qty", f.getName());
		assertEquals("1.9 Check Field Pos=3 actual=" + f.getPos(), 3 , f.getPos());
		assertEquals("1.A Check Field Type=8 actual=" + f.getType(), 8, f.getType());
	}
	
	private void checkCopybook(ExternalRecord copybook) {
		
		assertEquals("2.1 Check Sub records=2", 2, copybook.getNumberOfRecords());
		assertEquals("2.2 Check Fields=0; actual=" + copybook.getNumberOfRecordFields(), 0, copybook.getNumberOfRecordFields());
		assertEquals("2.3 Check File Structure=0; actual=" + copybook.getFileStructure(), 0, copybook.getFileStructure());
		assertEquals("2.4 Check Record Type=9; actual=" + copybook.getRecordType(), 9, copybook.getRecordType());
		assertEquals("2.5 Check Sep=<Tab>; actual=" + copybook.getDelimiter(), "<Tab>", copybook.getDelimiter());
		assertEquals("2.6 Check Style=0; actual=" + copybook.getRecordStyle(), 0, copybook.getRecordStyle());
		assertEquals("2.7 Check List=Y; actual=" + copybook.getListChar(), "Y", copybook.getListChar());
	}
	
	
	/**
	 * Compare the contents of a file and an array of String
	 * @param lines
	 * @param filename
	 * @throws IOException
	 */
	private void compareFile2Array(String[] lines, String filename) throws IOException {
		String[] newLines = readFile(filename);
		
		System.out.println(" Starting file: " + filename);
		
		for (int i = 0; i < lines.length; i++) {
			if (! lines[i].equals(newLines[i])) {
				System.out.println();
				System.out.println("Error file: " + filename + " line: " + i);
				System.out.println("   > " + lines[i]);
				System.out.println("   - " + newLines[i]);
				assertEquals("Error line " + i, lines[i], newLines[i]);
			}
		}
	}
	
	/**
	 * Read a file into an array of lines
	 * 
	 * @param fileName name of file to be read
	 * @return file contents
	 * 
	 * @throws IOException any error that occurred
	 */
	private String [] readFile(String fileName) throws IOException {
		ArrayList<String> lines = new ArrayList<String>();
		String[]ret; 
		String s;
		
		BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
		
		while ((s = r.readLine()) != null) {
			lines.add(s);
		}
		r.close();
		
		ret = new String[lines.size()];
		ret =  lines.toArray(ret);
		
		return ret;
	//	InputS
	}
}
