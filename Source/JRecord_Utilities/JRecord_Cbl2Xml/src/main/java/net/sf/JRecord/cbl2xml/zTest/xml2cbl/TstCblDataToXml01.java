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

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;

import org.junit.Test;
import org.xml.sax.SAXException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.def.Icb2xml2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.schema.jaxb.impl.AddPlusToNumeric;


public class TstCblDataToXml01 {

	private static final String[] EMPTY_STRING_ARRAY = {};
	
	private static final String LOC_DOWNLOAD_TXT = "Ams_LocDownload.txt";
	private String[][] files ={
			{"cb2xml_Output102a.xml", LOC_DOWNLOAD_TXT, "Ams_LocDownload_102.xml"},
			{"cb2xml_Output110.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_110.xml"},
			{"cb2xml_Output102.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_102.xml"},     //-- After updates for comments,
			{"cb2xml_Output111.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_111.xml"},
			{"cb2xml_Output112.xml",  "StoreSales5.txt",    "StoreSales5.xml"},          // -- Does not work 0.94
	};
	
	//private String firstLine = "TAR5839DCDC - Taras Ave                                                             30-68 Taras Ave                         Altona North                       3025      VICA";
	@Test
	public void testData2Xml() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		String copybookName, dataName, xmlDataName;
		for (String[] d : files) { 
			copybookName = Cb2XmlCode.getFullName("xmlCopybook/" + d[0]);
			dataName = Cb2XmlCode.getFullName(d[1]);
			
			byte[] doc = data2xml(dataName, copybookName, false);
			
			
			//System.out.println(XmlUtils.domToString(doc));
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			
			xmlDataName = Cb2XmlCode.getFullName("xml/" + d[2]);
			Cb2XmlCode.compare("File: " + copybookName,  xmlDataName, doc);
		}
	}
	
	@Test
	public void testData2XmlUseAddPo() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		String copybookName, dataName, xmlDataName;
		for (String[] d : files) { 
			copybookName = Cb2XmlCode.getFullName("xmlCopybook/" + d[0]);
			dataName = Cb2XmlCode.getFullName(d[1]);
			
			byte[] doc = data2xml(dataName, copybookName, true);
			
			
			//System.out.println(XmlUtils.domToString(doc));
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			
			xmlDataName = Cb2XmlCode.getFullName("xml/" + d[2]);
			String expectedXml = Cb2XmlCode.addPlusToNumeric(Cb2XmlCode.loadFile(xmlDataName, "\r\n", false), EMPTY_STRING_ARRAY);
			Cb2XmlCode.compareXmlStr("File: " + copybookName,  expectedXml, doc);
		}
	} 

	@Test
	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		String xml2data;
		String expected = loadLocationFile();
		
		for (int i = 0; i < files.length - 1; i++) { // xml2data only works when there are no arrays !!!
			String[] d = files[i];
			System.out.println("->> " + d[0]);
			xml2data = xml2data(Cb2XmlCode.getFullName("xml/" + d[2]), Cb2XmlCode.getFullName("xmlCopybook/" + d[0]));
			//System.out.println(xml2data);
			Cb2XmlCode.compare(d[0] + " " +  i, expected, xml2data);
			//assertEquals(d[0] + " " +  i, expected, xml2data);
		}
	}
	

	
	private static byte[] data2xml(String dataFileName, String copybookFileName, boolean addPlusToNums) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		Icb2xml2Xml xmlWriter = Cobol2GroupXml.newCb2Xml2Xml(copybookFileName)
					  .setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					  .setXmlMainElement("copybook");
		
		if (addPlusToNums) {
			xmlWriter.setFormatField(AddPlusToNumeric.INSTANCE);
		}
		
		xmlWriter
					  .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	
	
	
	private static String xml2data(String dataFileName, String copybookFileName) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		Cobol2GroupXml.newCb2Xml2Xml(copybookFileName)
					  .setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					  .xml2Cobol(new FileInputStream(dataFileName), os);
		return new String(os.toByteArray());

	}
	
	private static String loadLocationFile() throws IOException {
		return Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("compare/" + LOC_DOWNLOAD_TXT), "\r\n", true);
	}
}
