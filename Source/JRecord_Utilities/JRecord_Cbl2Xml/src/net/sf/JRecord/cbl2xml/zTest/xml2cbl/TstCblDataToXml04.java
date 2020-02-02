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
import java.io.InputStream;


import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.schema.IArrayItemCheck;

import org.junit.Test;
import org.xml.sax.SAXException;

public class TstCblDataToXml04 {

	private String[][] files ={
			{"amsPoDownload.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownload.xml", "1", "01", "Ams_PODownload_20041231_cbl.txt"},
			{"amsPoDownload.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownloadSkip.xml", "2", "01", "Ams_PODownload_20041231_space.txt"},
			{"amsPoDownload.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownloadSkip.xml", "3", "01", "Ams_PODownload_20041231_space.txt"},
			{"amsPoDownload_Count.cbl",  "Ams_PODownload_20041231_count.txt",  "amsPoDownloadCount.xml", "4", "01", "Ams_PODownload_20041231_count.txt"},
	};
	private String[][] filesFromXml={
			{"amsPoDownload_Count.cbl",  "Ams_PODownload_20041231_count.txt",  "amsPoDownloadSkip.xml", "4", "01", "Ams_PODownload_20041231_count.txt"},
	};

	boolean isBinary;

	@Test
	public void testData2Xml01() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
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
		String xmlDataName;
		
		System.out.println("Checking: " + d[0]);
		copybookName = Cb2XmlCode.getFullName("cobol/" + d[0]);
		dataName = Cb2XmlCode.getFullName(d[1]);

		byte[] doc;
		doc = data2xml1(dataName, copybookName, false, d[4], d[3]);

		System.out.println("Copybook: " + d[0] + " " + d[2]  + "  " + d[3]);
		System.out.println();
		//System.out.println(new String(doc));

		xmlDataName = Cb2XmlCode.getFullName("xml/" + d[2]);
		Cb2XmlCode.compare("File: " + copybookName,  xmlDataName, doc);
	} 
	
	 
	
	private static byte[] data2xml1(String dataFileName, String copybookFileName, boolean dropCopybookName, String splitId, String checkId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);		
		
		createXmlBuilder(copybookFileName, splitId, checkId)
					      .setRecordSelection("PO-Record",       newFieldSelection("Record-Type","H1"))
					      .setRecordSelection("Product-Record",  newFieldSelection("Record-Type","D1"))
					      .setRecordSelection("Location-Record", newFieldSelection("Record-Type","S1"))
				      .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	



	/**
	 * @param copybookFileName
	 * @return
	 */
	private static ICobol2Xml createXmlBuilder(String copybookFileName, String splitId, String checkId) {
		int split = CopybookLoader.SPLIT_01_LEVEL;
		if ("N".equals(splitId)) {
			split = CopybookLoader.SPLIT_NONE;
		} else if (! "01".equals(splitId)) {
			split = CopybookLoader.SPLIT_HIGHEST_REPEATING;
		}
		
		IArrayItemCheck check;
		if ("1".equals(checkId)) {
			check = Cobol2Xml.ARRAY_CHECK_BUILDER.newStopAtSpaces();
		} else if ("2".equals(checkId)) {
			check = Cobol2Xml.ARRAY_CHECK_BUILDER.newStopAtSpacesZeros();
		} else if ("3".equals(checkId)) {
			check = Cobol2Xml.ARRAY_CHECK_BUILDER.newSkipSpacesZeros();
		} else if ("4".equals(checkId)) {
			check = Cobol2Xml.ARRAY_CHECK_BUILDER.newIndexCheck("Array-Count");
		} else{
			check = Cobol2Xml.ARRAY_CHECK_BUILDER.newStopAtSpaces();		
		}

		return Cobol2GroupXml.newCobol2Xml(copybookFileName)
					      .setFileOrganization(Constants.IO_BIN_TEXT)
					      .setDialect(ICopybookDialects.FMT_INTEL)
					      .setSplitCopybook(split)
					      .setArrayCheck("location", check);
	}

    private static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
	@Test
	public void testXml2Data01() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		tstXml2Data(files);
	}
	
	@Test
	public void testXml2Data02() throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		tstXml2Data(filesFromXml);
	}

	public void tstXml2Data(String[][] files) throws IOException, SAXException, ParserConfigurationException, RecordException, XMLStreamException {
		byte[] xml2data;
		byte[][] expected = new byte[files.length][];
		String[] d = files[0];
		String fn;
		
		for (int i = 0; i < files.length; i++) {
			fn = files[i].length<6 
					?files[i][1]
					:files[i][5];
			expected[i] = readFile(Cb2XmlCode.getFullName(fn));
		}
		for (int i = 0; i < files.length; i++) { // xml2data only works when there are no arrays !!!
			try {
				d = files[i];
				System.out.println("->> " + d[0] + " " + d[2]);
				xml2data = xml2data1(
						Cb2XmlCode.getFullName("xml/" + d[2]), 
						Cb2XmlCode.getFullName("cobol/" + d[0]),
						d[4],
						d[3]);
				System.out.println("=== " +i + " " + expected[i].length + " " + xml2data.length + " " + isBinary);
				Cb2XmlCode.compare("idx=" + i, isBinary, xml2data, expected[i]);
			} catch (Exception e) {
				System.err.println();
				System.err.println("   --> " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
				System.err.println();
				throw new RuntimeException(e);
			}
		}
	}





	@SuppressWarnings("unchecked")
	private byte[] xml2data1(String dataFileName, String copybookFileName, String splitId, String checkId) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		
		ICobol2Xml xmlBldr = createXmlBuilder(copybookFileName, splitId, checkId);
		xmlBldr	  .xml2Cobol(new FileInputStream(dataFileName), os);
		
		isBinary = ((CblIOBuilderMultiSchemaBase<ICobol2Xml> )xmlBldr).getLayout().isBinary();

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
