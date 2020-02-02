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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;


import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.junit.Test;
import org.xml.sax.SAXException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.Option.Options;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;

public class TstCblDataToXml23 {

	private String[][] files ={
			{"ArrayDependingCopybook.cbl",  "ArrayDependingFile.txt",  "ArrayDependingFile.xml", "3", "N"},
			{"ArrayCopybook.cbl",  "ArrayFile.txt",  "ArrayFile.xml", "3", "N"},
			{"hdt.cbl",  "hdt.txt",  "hdt.xml", "2", "01"},
			{"hdt05.cbl",  "hdt.txt",  "hdt.xml", "2", "HR"},
			{"amsPoDownload.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownload.xml", "1", "01"},
			{"amsPoDownload05.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownload.xml", "1", "HR"},
	};
	private static final int[] TAG_FORMATS = {
		IReformatFieldNames.RO_LEAVE_ASIS, IReformatFieldNames.RO_UNDERSCORE, IReformatFieldNames.RO_CAMEL_CASE
	};


	private int tagFormat;
	boolean isBinary;
	
	//private String firstLine = "TAR5839DCDC - Taras Ave                                                             30-68 Taras Ave                         Altona North                       3025      VICA";
	@Test
	public void testData2Xml() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		for (String[] d : files) {
			checkData2Xml(d);
		}
	}
	


	/**
	 * @param d
	 * @throws FileNotFoundException
	 * @throws RecordException
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	private void checkData2Xml(String[] d) throws FileNotFoundException,
			RecordException, IOException, XMLStreamException,
			SAXException, ParserConfigurationException,
			FactoryConfigurationError {
		String copybookName;
		String dataName;
		String xmlData = Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("xml/" + d[2]), "\r\n", false);
		
		for (int tf : TAG_FORMATS) {
			tagFormat = tf;
			System.out.println("Checking: " + d[0] + ", " + tf);
			copybookName = Cb2XmlCode.getFullName("cobol/" + d[0]);
			dataName = Cb2XmlCode.getFullName(d[1]);
	
			byte[] doc;
			if ("1".equals(d[3])) {
				doc = data2xml1(dataName, copybookName, "Y".equalsIgnoreCase(d[3]), d[4]);
			} else if ("2".equals(d[3])) {
				doc = data2xml2(dataName, copybookName, "Y".equalsIgnoreCase(d[3]), d[4]);
			} else {
				doc = data2xml3(dataName, copybookName, "Y".equalsIgnoreCase(d[3]), d[4]);
			}
	
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			System.out.println(new String(doc));
	
			Cb2XmlCode.compareXmlStr("File: " + tf + ", " + copybookName,  ReformatXml.reformaXml(tf, xmlData), doc);
		}
	} 
	
	private byte[] data2xml1(String dataFileName, String copybookFileName, boolean dropCopybookName, String splitId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);		
		
		createXmlBuilder(copybookFileName, splitId)
					      .setRecordSelection("PO-Record", newFieldSelection("Record-Type","H1"))
					      .setRecordSelection("Product-Record", newFieldSelection("Record-Type","D1"))
					      .setRecordSelection("Location-Record", newFieldSelection("Record-Type","S1"))
				      .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	

	private byte[] data2xml2(String dataFileName, String copybookFileName, boolean dropCopybookName, String splitId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {

		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);

		createXmlBuilder(copybookFileName, splitId)
					      .setRecordPositionCode("Header-Record",  Options.RP_FIRST_RECORD_IN_FILE)
					      .setRecordPositionCode("Detail-Record",  Options.RP_MIDDLE_RECORDS)
					      .setRecordPositionCode("Trailer-Record", Options.RP_LAST_RECORD_IN_FILE)
				      .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}


	private byte[] data2xml3(String dataFileName, String copybookFileName, boolean dropCopybookName, String splitId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);

				
		createXmlBuilder(copybookFileName, splitId)
				      .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}


	/**
	 * @param copybookFileName
	 * @return
	 */
	private ICobol2Xml createXmlBuilder(String copybookFileName, String splitId) {
		int split = CopybookLoader.SPLIT_01_LEVEL;
		if ("N".equals(splitId)) {
			split = CopybookLoader.SPLIT_NONE;
		} else if (! "01".equals(splitId)) {
			split = CopybookLoader.SPLIT_HIGHEST_REPEATING;
		}
		return Cobol2GroupXml.newCobol2Xml(copybookFileName)
					      .setFileOrganization(Constants.IO_BIN_TEXT)
					      .setDialect(ICopybookDialects.FMT_FUJITSU)
					      .setSplitCopybook(split)
					      .setTagFormat(tagFormat);
	}

    private static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
	@Test
	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		byte[] xml2data;
		byte[][] expected = new byte[files.length][];
		String[] d = files[0];
		
		for (int i = 0; i < files.length; i++) {
			expected[i] = readFile(Cb2XmlCode.getFullName(files[i][1]));
		}
		for (int i = 0; i < files.length - 2; i++) { // xml2data only works when there are no arrays !!!
			d = files[i];
			String xmlStr = Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("xml/" + d[2]), "\r\n", false);
			for (int tf : TAG_FORMATS) {
				tagFormat = tf;
				try {
					System.out.println("->> " + tf + " " + d[0] + " " + d[2]);
					xml2data = xml2data1(
							new ByteArrayInputStream(
									ReformatXml.reformaXml(tf, xmlStr).getBytes()
									), 
							Cb2XmlCode.getFullName("cobol/" + d[0]),
							d[4]);
					System.out.println("=== " + expected[i].length + " " + xml2data.length);
	 				Cb2XmlCode.compare("Tag: " + tf, isBinary, xml2data, expected[i]);

//					if (expected[i].length == xml2data.length - 2) {
//						for (int k = 0; k < expected[i].length; k++) {
//							Assert.assertEquals("idx=" + i + ", " + k, expected[i][k], xml2data[k] );
//						}
//					} else {
//						assertArrayEquals("idx=" + i, expected[i], xml2data);
//					}
				} catch (RuntimeException e) {
					System.err.println();
					System.err.println("   --> " + tf + ", " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
					System.err.println();
					throw e;
				} catch (Exception e) {
					System.err.println();
					System.err.println("   --> " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
					System.err.println();
					throw new RuntimeException(e);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private byte[] xml2data1(InputStream xmlStream, String copybookFileName, String splitId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException {
		 
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		
		ICobol2Xml xmlBldr = createXmlBuilder(copybookFileName, splitId);
		xmlBldr	  .xml2Cobol(xmlStream, os);
		isBinary = ((CblIOBuilderMultiSchemaBase<ICobol2Xml> )xmlBldr).getLayout().isBinary();

		
//		Cobol2GroupXml.newCobol2Xml(copybookFileName)
//					      .setFileOrganization(fileOrg)
//					      .setDialect(ICopybookDialects.FMT_INTEL)
//					      .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
//					      .setRecordSelection("PO-Record", newFieldSelection("Record-Type","H1"))
//					      .setRecordSelection("Product-Record", newFieldSelection("Record-Type","D1"))
//					      .setRecordSelection("Location-Record", newFieldSelection("Record-Type","S1"))
//					  .xml2Cobol(new FileInputStream(dataFileName), os);
		return os.toByteArray();
	}
	

	private byte[] readFile(String fileName) throws IOException {
		InputStream is = new FileInputStream(fileName);
		ByteArrayOutputStream os = new ByteArrayOutputStream(4000);
		byte[] buf = new byte[4000];
		
		int len = is.read(buf);
		while (len > 0) {
			os.write(buf, 0, len);
			len = is.read(buf);
		}
		is.close();
		
		return os.toByteArray();
		
		
	}

}
