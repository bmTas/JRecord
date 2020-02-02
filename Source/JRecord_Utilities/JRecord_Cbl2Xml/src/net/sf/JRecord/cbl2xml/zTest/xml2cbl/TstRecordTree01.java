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

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.cobol.HierarchyRecordDef;

import org.junit.Test;
import org.xml.sax.SAXException;

public class TstRecordTree01 {

	private String[][] files ={
			{"MRHF.cbl",           "MRHF.txt",                     "MRHF_Tree.xml",         "2", "01"},
			{"amsPoDownload.cbl",  "Ams_PODownload_20041231.txt",  "amsPoDownloadTree.xml", "1", "01"},
	};

	int tagFormat = -1;		
	boolean isBinary;
	
	private static final int[] TAG_FORMATS = {
		-1, IReformatFieldNames.RO_LEAVE_ASIS, IReformatFieldNames.RO_UNDERSCORE, IReformatFieldNames.RO_CAMEL_CASE
	};

	
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

		String xmlStr = Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("xml/" + d[2]), "\r\n", false);
		
		for (int tf : TAG_FORMATS) {
			tagFormat = tf;
			System.out.println("Checking: " + d[0] + " " + tf);
			copybookName = Cb2XmlCode.getFullName("cobol/" + d[0]);
			dataName = Cb2XmlCode.getFullName(d[1]);
	

			ICobol2Xml toXml;
			
			toXml = createXmlBldr(d[3], copybookName);
			byte[] doc = data2xml1(toXml, dataName, copybookName, "Y".equalsIgnoreCase(d[3]));
			
	
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			System.out.println(new String(doc));
	
			Cb2XmlCode.compareXmlStr("File: " + copybookName,  ReformatXml.reformaXml(tf, xmlStr), doc);
		}
	}


	/**
	 * @param d
	 * @param copybookName
	 * @return
	 * @throws IOException
	 */
	private ICobol2Xml createXmlBldr(String id, String copybookName)
			throws IOException {
		ICobol2Xml toXml;
		if ("2".equals(id)) {
			toXml = createXmlBuilder2(copybookName);
		} else {
			toXml = createXmlBuilder1(copybookName);
		}
		return toXml;
	} 

	
	private byte[] data2xml1(ICobol2Xml toXml, String dataFileName, String copybookFileName, boolean dropCopybookName) 
	throws FileNotFoundException, RecordException, IOException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);		
		
		toXml  .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}

	

	/**
	 * @param copybookFileName
	 * @return
	 */
	private  ICobol2Xml createXmlBuilder1(String copybookFileName) {
		
		ICobol2Xml ret = Cobol2GroupXml
								.newCobol2Xml(copybookFileName)
								      .setFileOrganization(Constants.IO_UNICODE_TEXT)
								      .setDialect(ICopybookDialects.FMT_INTEL)
								      .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL) 
								      
								      .setRecordSelection("PO-Record",       newFieldSelection("Record-Type","H1"))
								      .setRecordSelection("Product-Record",  newFieldSelection("Record-Type","D1"))
								      .setRecordSelection("Location-Record", newFieldSelection("Record-Type","S1"))
								      
								      .setRecordParent("Product-Record",  "PO-Record")
								      .setRecordParent("Location-Record", "Product-Record");
		
		if (tagFormat >= 0) {
			ret.setTagFormat(tagFormat);
		}
		
		return ret;
	}
	
	private  ICobol2Xml createXmlBuilder2(String copybookFileName) throws IOException {
		int split = CopybookLoader.SPLIT_01_LEVEL;
		
		ICobol2Xml ret = Cobol2GroupXml.newCobol2Xml(copybookFileName)
					      .setFileOrganization(Constants.IO_UNICODE_TEXT)
					      .setDialect(ICopybookDialects.FMT_MAINFRAME)
					      .setSplitCopybook(split);
		@SuppressWarnings("unchecked")
		HierarchyRecordDef def = HierarchyRecordDef.getMrhfRecordDef(((CblIOBuilderMultiSchemaBase<ICobol2Xml>) ret).getLayout());
		
		updateRecordDef(ret, def);
		if (tagFormat >= 0) {
			ret.setTagFormat(tagFormat);
		}
		
		return ret;
	}

	private void updateRecordDef(ICobol2Xml toXml, HierarchyRecordDef def) {
		
		if (def.children != null) {
			for (HierarchyRecordDef c : def.children) {
				toXml.setRecordSelection(c.name, newFieldSelection("Record-Type", Integer.toString(c.recId)));
				if (def.getRecIdx() >= 0) {
					toXml.setRecordParent(c.name, def.name);
				}
				updateRecordDef(toXml, c);
			}
		}
	}

    private static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
    
 	@Test
 	public void testXml2Data01() throws IOException, SAXException, ParserConfigurationException, XMLStreamException {
 		byte[] xml2data;
 		byte[] expected ;
 		
 		tagFormat = -1;

 		String[] d = files[0];
 		for (int i = 0; i < files.length; i++) { // xml2data only works when there are no arrays !!!
 			try {
 				d = files[i];
 				String xmlStr = Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("xml/" + d[2]), false);
 				expected = readFile(Cb2XmlCode.getFullName(files[i][1]));
 				
 				for (int tf : TAG_FORMATS) {
 					tagFormat = tf;
	 				System.out.println("->> " + tf + ", " + d[0] + " " + d[2]);
	 				xml2data = xml2data1(
	 						d[3],
	 						new ByteArrayInputStream(ReformatXml.reformaXml(tagFormat, xmlStr).getBytes()), 
	 						Cb2XmlCode.getFullName("cobol/" + d[0]),
	 						d[4]);
	 				System.out.println("=== " + tf + ", " + expected.length + " " + xml2data.length);
	 				Cb2XmlCode.compare("Tag: " + tf, isBinary, xml2data, expected);
//	 				if (expected.length == xml2data.length - 2) {
//	 					for (int k = 0; k < expected.length; k++) {
//	 						Assert.assertEquals("idx=" + i + ", " + k, expected[k], xml2data[k] );
//	 					}
//	 				} else {
//						System.out.println(new String(xml2data));
//						assertEquals("idx=" + i, new String(expected), new String(xml2data));
//	 				}
 				}
 			} catch (RuntimeException e) {
 				System.err.println();
 				System.err.println("   --> " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
 				System.err.println();
 				throw e;
 			}
 		}
 	}

 	@SuppressWarnings("unchecked")
	private byte[] xml2data1(String id, InputStream xmlStream, String copybookFileName, String splitId) 
 	throws FileNotFoundException, RecordException, IOException, XMLStreamException {
 		
 		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
 		
 		ICobol2Xml xmlBldr = createXmlBldr(id, copybookFileName);
		xmlBldr	  .xml2Cobol(xmlStream, os);
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
