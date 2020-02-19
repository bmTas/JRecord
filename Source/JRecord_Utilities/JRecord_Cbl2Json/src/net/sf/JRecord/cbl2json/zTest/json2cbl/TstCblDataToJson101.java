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

package net.sf.JRecord.cbl2json.zTest.json2cbl;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;









import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cobolToJson.impl.Cobol2JsonImp;

import org.junit.Test;


public class TstCblDataToJson101 {
	private String[][] files01 ={
			{"DTAR020.cbl",  "DTAR020_tst1.bin",  "DTAR020_tst1_DataN.json", "N", "F"},
			{"FCUSDAT.cbl",  "ZOS.FCUSTDAT_150.vb.bin", "ZOS.FCUSTDAT_150.json","N", "V"},
			{"DTAR020.cbl",  "DTAR020.bin",  "DTAR020_DataN.json", "N", "F"},
			{"DTAR107.cbl",  "DTAR107.bin",  "DTAR107_DataN.json", "N", "F"},
//			{"DTAR192.cbl",  "DTAR192.bin",  "DTAR192_DataN.json", "N", "F"},
			{"DTAR1000.cbl", "DTAR1000.bin", "DTAR1000_DataN.json","N", "V"},
			{"FCUSDAT.cbl",  "ZOS.FCUSTDAT_150.vb.bin", "ZOS.FCUSTDAT_150.json","N", "V"},
			{"DTAR020.cbl",  "DTAR020.bin",  "DTAR020_DataY.json", "Y", "F"},
			{"DTAR107.cbl",  "DTAR107.bin",  "DTAR107_DataY.json", "Y", "F"},
//			{"DTAR192.cbl",  "DTAR192.bin",  "DTAR192_DataY.json", "Y", "F"},
			{"DTAR1000.cbl", "DTAR1000.bin", "DTAR1000_DataY.json","Y", "V"},
	};
	


	
	private static final int[] TAG_FORMATS = {
		IReformatFieldNames.RO_UNDERSCORE, IReformatFieldNames.RO_LEAVE_ASIS, //IReformatFieldNames.RO_CAMEL_CASE
	};


	private int tagFormat;

	//private String firstLine = "TAR5839DCDC - Taras Ave                                                             30-68 Taras Ave                         Altona North                       3025      VICA";
	@Test
	public void testData2Json01() throws IOException {
		for (String[] d : files01) {
			check(d);
		}
	}
	@Test
	public void testData2JsonCC01() throws IOException {
		for (String[] d : files01) {
			checkCamelCase(d);
		}
//		checkCamelCase(files01[0]);
	}

	public void testData2Json_DTAR107() throws IOException {
		check(files01[4]);
	}

	private void check(String[] d) throws IOException	 {
		String copybookName;
		String dataName;

		String jsonData = Cbl2JsonCode.loadFile(Cbl2JsonCode.getFullName("json/" + d[2]), "\r\n", false);
		
		for (int tf : TAG_FORMATS) {
			tagFormat = tf;
			System.out.println("Checking: " + d[0] + ", " + tf);
			copybookName = Cbl2JsonCode.getFullName("cobol/" + d[0]);
			dataName = Cbl2JsonCode.getFullName(d[1]);
			
			byte[] doc = data2json(dataName, copybookName, "Y".equalsIgnoreCase(d[3]),  d[4]);
			
			
			//System.out.println(XmlUtils.domToString(doc));
			System.out.println("Copybook: " + d[0] + " " + d[2]);
//			System.out.println();
//			System.out.println(new String(doc));
		
//		xmlDataName = Cb2XmlCode.getFullName("xml/" + d[2]);
//		Cb2XmlCode.compare("File: " + copybookName,  xmlDataName, doc);
			Cbl2JsonCode.compareXmlStr("File: " + tf + ", " + copybookName,  ReformatJson.reformatJson(tf, jsonData), doc);
		}
	} 
	

	private void checkCamelCase(String[] d) throws IOException	 {
		String copybookName;
		String dataName;

		String jsonData = Cbl2JsonCode.loadFile(Cbl2JsonCode.getFullName("json/" + d[2]), "\r\n", false);
		

		tagFormat = IReformatFieldNames.RO_CAMEL_CASE;
		System.out.println("Checking: " + d[0] + ", " + tagFormat);
		copybookName = Cbl2JsonCode.getFullName("cobol/" + d[0]);
		dataName = Cbl2JsonCode.getFullName(d[1]);
		
		byte[] doc = data2json(dataName, copybookName, "Y".equalsIgnoreCase(d[3]),  d[4]);
		
		
		//System.out.println(XmlUtils.domToString(doc));
		System.out.println("Copybook: " + d[0] + " " + d[2]);
//		System.out.println();
//		System.out.println(new String(doc));
	
		String json = new String(doc);
		String updJson = 
				Conversion.replace(
						Conversion.replace(jsonData, "_", ""),
						"\r", ""
				).toString();
		if (! updJson.equalsIgnoreCase(Conversion.replace(json, "\r", "").toString())) {
			org.junit.Assert.assertEquals(d[0], updJson, json);
		}
	} 

	
	private byte[] data2json(String dataFileName, String copybookFileName, boolean dropCopybookName, String fileType) 
	throws  IOException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		int fileOrg = Constants.IO_FIXED_LENGTH;
		if ("V".equalsIgnoreCase(fileType)) {
			fileOrg = Constants.IO_VB;
		}
		
		Cobol2JsonImp.newCobol2Json(copybookFileName)
					  .setFileOrganization(fileOrg)
					  .setFont("cp037")
					  .setDialect(ICopybookDialects.FMT_MAINFRAME) // This is the default
					  .setTagFormat(tagFormat)
					  .setDropCopybookNameFromFields(dropCopybookName)
					  .cobol2json(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	
	
//	@Test
//	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException, RecordException,  XMLStreamException {
//		byte[] xml2data;
//		int size = files.length / 2;
//		byte[][] expected = new byte[size][];
//		String[] d = files[0];
//		
//		for (int i = 0; i < expected.length; i++) {
//			expected[i] = readFile(Cb2XmlCode.getFullName(files[i][1]));
//		}
//		for (int i = 0; i < files.length; i++) { // xml2data only works when there are no arrays !!!
//			d = files[i];
//			if (d[1].indexOf("DTAR107") < 0) {
//				String xmlStr = Cb2XmlCode.loadFile(Cb2XmlCode.getFullName("xml/" + d[2]), "\r\n", false);
//				for (int tf : TAG_FORMATS) {
//					tagFormat = tf;
//					try {
//	
//						System.out.println("->> " + d[0] + " " + d[2]);
//						xml2data = xml2data(
//								new ByteArrayInputStream(
//									ReformatXml.reformaXml(tagFormat, xmlStr).getBytes()	
//								), 
//								Cb2XmlCode.getFullName("cobol/" + d[0]), 
//								"Y".equalsIgnoreCase(d[3]),  "V".equalsIgnoreCase(d[4]));
//						//System.out.println(xml2data);
//						assertArrayEquals("idx=" + 1, expected[i%size], xml2data);
//					} catch (RuntimeException e) {
//						System.err.println();
//						System.err.println("   --> " + tf + ", " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
//						System.err.println();
//						throw e;
//					} catch (Exception e) {
//						System.err.println();
//						System.err.println("   --> " + tf + ", " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
//						System.err.println(ReformatXml.reformaXml(tagFormat, xmlStr));
//						throw new RuntimeException(e);
//					}
//				}
//			}
//		}
//	}
//
//	private byte[] xml2data(InputStream dataStream, String copybookFileName, boolean dropCopybookName, boolean vb) 
//	throws FileNotFoundException, RecordException, IOException,  XMLStreamException {
//		
//		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
//		int fileOrg = Constants.IO_FIXED_LENGTH;
//		if (vb) {
//			fileOrg = Constants.IO_VB;
//		}
//
//		Cobol2GroupXml.newCobol2Xml(copybookFileName)
//					  .setFileOrganization(fileOrg)
//					  .setXmlMainElement("copybook")
//					  .setFont("cp037")
//					  .setDropCopybookNameFromFields(dropCopybookName)
//					  .setTagFormat(tagFormat)
//					  .xml2Cobol(dataStream, os);
//		return os.toByteArray();
//
//	}
	
//	private byte[] readFile(String fileName) throws IOException {
//		InputStream is = new FileInputStream(fileName);
//		ByteArrayOutputStream os = new ByteArrayOutputStream(4000);
//		byte[] buf = new byte[4000];
//		
//		int len = is.read(buf);
//		while (len > 0) {
//			os.write(buf, 0, len);
//			len = is.read(buf);
//		}
//		is.close();
//		
//		return os.toByteArray();	
//	}

}
