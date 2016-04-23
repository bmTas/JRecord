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

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.ConvertOptions;
import junit.framework.TestCase;

public class TstConvertOptions extends TestCase {
	String cbl1 = "amsPoDownload.cbl";
	String forgText = "text";
	String poRec = "PO-Record";
	String recType = "Record-Type";
	String productRec = "Product-Record";
	String locationRec = "Location-Record";
	String poValue = "H1";
	String productValue = "D1";
	String storeValue = "S1";
	String in1 = "in/Ams_PODownload_20041231.txt";
	String out1 = "out/Ams_PODownload_20041231_bat_Tree.xml";
	
	String dtar020 = "DTAR020b.cbl";


	public void test01() {
		
		String[] args1 = {
				"-cobol", cbl1, "-fileOrganisation", forgText,
				 "-recordSelection", poRec,       recType + "=" + poValue,
				 "-recordSelection", productRec,  recType + "=" + productValue, "-recordParent", productRec, poRec,
				 "-recordSelection", locationRec, recType + "=" + storeValue,   "-recordParent", locationRec, productRec,
				 "-input", in1,
				 "-output", out1,
		};

		checkPoTree(new ConvertOptions(args1), ICopybookDialects.FMT_MAINFRAME, IReformatFieldNames.RO_LEAVE_ASIS, Constants.IO_BIN_TEXT);
	}
	

	public void test02() {
		String[] args1 = {
				"-cobol", cbl1, "-fileOrganisation", "FixedWidth", 
				"-dialect", "GNUCobol", "-tagFormat", "_",
				 "-recordSelection", poRec,       recType + " " + poValue,
				 "-recordSelection", productRec,  recType + " " + productValue, "-recordParent", productRec, poRec,
				 "-recordSelection", locationRec, recType + " " + storeValue,   "-recordParent", locationRec, productRec,
				 "-input", in1,
				 "-output", out1,
		};

		checkPoTree(new ConvertOptions(args1), ICopybookDialects.FMT_GNU_COBOL, IReformatFieldNames.RO_UNDERSCORE, Constants.IO_FIXED_LENGTH);
	}


	public void test03() {
		String[] args1 = {
				"-cobol", cbl1, "-fileOrganisation", "Mainframe_VB", 
				"-dialect", "Futjitsu", "-tagFormat", "C",
				 "-recordSelection", poRec,       recType + " = " + poValue,
				 "-recordSelection", productRec,  recType + " " + productValue, "-recordParent", productRec, poRec,
				 "-recordSelection", locationRec, recType + "=" + storeValue,   "-recordParent", locationRec, productRec,
				 "-input", in1,
				 "-output", out1,
		};

		checkPoTree(new ConvertOptions(args1), ICopybookDialects.FMT_FUJITSU, IReformatFieldNames.RO_CAMEL_CASE, Constants.IO_VB);
	}


	public void test11() {
		String font = "cp037";
		String in = "in/DTAR020.bin";
		String out = "out/DTAR020_A.xml";
		
		String[] args1 = {
				 "-cobol", dtar020, "-font", font, "-fileOrganisation", "FixedWidth",
				 "-input", in, 
				 "-output", out, 
		};

		checkDTAR020(font, in, out, false, ICobol2Xml.MAIN_XML_TAG, new ConvertOptions(args1));
	}


	public void test12() {
		String font = "cp273";
		String in = "in/DTAR020b.bin";
		String out = "out/DTAR020_B.xml";
		
		String mainTag = "DTAR020_File";
		String[] args1 = {
				 "-cobol", dtar020, "-font", font, "-fileOrganisation", "FixedWidth",
				 ConvertOptions.OPT_MAIN_XML_TAG, mainTag, ConvertOptions.OPT_DROP_COPYBOOK_NAME, "true",
				 "-input", in, 
				 "-output", out, 
		};

		checkDTAR020(font, in, out, true, mainTag, new ConvertOptions(args1));
	}

	public void test21() {
		String font = "cp273";
		String in = "in/DTAR020b.bin";
		String out = "out/DTAR020_B.xml";
		String mainTag = "ddd_File";
		
		String[] args1 = {
				 "-cb2xml", "ddd.xl", "-font", font, "-fileOrganisation", "FixedWidth",
				 ConvertOptions.OPT_MAIN_XML_TAG, mainTag, ConvertOptions.OPT_DROP_COPYBOOK_NAME, "false",
				 "-input", in, 
				 "-output", out, 
		};

		ConvertOptions opts = new ConvertOptions(args1);
		
		assertEquals("", opts.cobolCopybook);
		assertEquals( "ddd.xl", opts.cb2xmlCopybook);
		assertEquals(false, opts.useCobol);
		
		
		checkDTAR020a(font, in, out, false, mainTag, opts);
	}

	/**
	 * @param cbl
	 * @param font
	 * @param in
	 * @param out
	 * @param dropCopybookName
	 * @param mainXmlTag
	 * @param opts
	 */
	private void checkDTAR020(String font, String in, String out,
			boolean dropCopybookName, String mainXmlTag, ConvertOptions opts) {
		assertEquals(dtar020, opts.cobolCopybook);
		assertEquals("", opts.cb2xmlCopybook);
		assertEquals(true, opts.useCobol);
		
		checkDTAR020a(font, in, out, dropCopybookName, mainXmlTag, opts);
	}
	
	private void checkDTAR020a(String font, String in, String out,
			boolean dropCopybookName, String mainXmlTag, ConvertOptions opts) {
		assertEquals(ICopybookDialects.FMT_MAINFRAME, opts.dialect);
		assertEquals(dropCopybookName, opts.dropCopybookName);
		assertEquals(Constants.IO_FIXED_LENGTH, opts.fileOrganisation);
		assertEquals(font, opts.font);
		assertEquals(in, opts.inputFile);
		assertEquals(mainXmlTag, opts.mainXmlTag);
		assertEquals(out, opts.outputFile);
		assertEquals(IReformatFieldNames.RO_LEAVE_ASIS, opts.tagFormat);
		assertEquals(true, opts.isOk());
		assertEquals(0, opts.recordParents.size());
		assertEquals(0, opts.recordSelect.size());
	}

	/**
	 * @param opts
	 * @param dialect
	 * @param tagFormat
	 */
	private void checkPoTree(ConvertOptions opts, int dialect, int tagFormat, int fOrg) {
		assertEquals(cbl1, opts.cobolCopybook);
		assertEquals("", opts.cb2xmlCopybook);
		assertEquals(dialect, opts.dialect);
		assertEquals(false, opts.dropCopybookName);
		assertEquals(fOrg, opts.fileOrganisation);
		assertEquals("", opts.font);
		assertEquals(in1, opts.inputFile);
		assertEquals(ICobol2Xml.MAIN_XML_TAG, opts.mainXmlTag);
		assertEquals(out1, opts.outputFile);
		assertEquals(tagFormat, opts.tagFormat);
		assertEquals(true, opts.useCobol);
		assertEquals(true, opts.isOk());
		assertEquals(2, opts.recordParents.size());
		assertEquals(3, opts.recordSelect.size());
		
		assertEquals(productRec, opts.recordParents.get(0).recordName);
		assertEquals(poRec, opts.recordParents.get(0).parentName);
		assertEquals(locationRec, opts.recordParents.get(1).recordName);
		assertEquals(productRec, opts.recordParents.get(1).parentName);
		
		assertEquals(poRec, opts.recordSelect.get(0).recordName);
		assertEquals(recType, opts.recordSelect.get(0).fieldName);
		assertEquals(poValue, opts.recordSelect.get(0).value);
		
		assertEquals(productRec, opts.recordSelect.get(1).recordName);
		assertEquals(recType, opts.recordSelect.get(1).fieldName);
		assertEquals(productValue, opts.recordSelect.get(1).value);
		
		assertEquals(locationRec, opts.recordSelect.get(2).recordName);
		assertEquals(recType, opts.recordSelect.get(2).fieldName);
		assertEquals(storeValue, opts.recordSelect.get(2).value);

	}
}
