package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.*;


import java.io.ByteArrayOutputStream;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;


import org.junit.Test;
import org.xml.sax.SAXException;


public class TstCblDataToXml01 {

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
	public void testData2Xml() throws IOException, SAXException, ParserConfigurationException, RecordException, JAXBException, XMLStreamException {
		String copybookName, dataName, xmlDataName;
		for (String[] d : files) {
			copybookName = Code.getFullName("xmlCopybook/" + d[0]);
			dataName = Code.getFullName(d[1]);
			
			byte[] doc = data2xml(dataName, copybookName);
			
			
			//System.out.println(XmlUtils.domToString(doc));
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			
			xmlDataName = Code.getFullName("xml/" + d[2]);
			Code.compare("File: " + copybookName,  xmlDataName, doc);
		}
	} 

	@Test
	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException, RecordException, JAXBException, XMLStreamException {
		String xml2data;
		String expected = loadLocationFile();
		
		for (int i = 0; i < 3; i++) { // xml2data only works when there are no arrays !!!
			String[] d = files[i];
			System.out.println("->> " + d[0]);
			xml2data = xml2data(Code.getFullName("xml/" + d[2]), Code.getFullName("xmlCopybook/" + d[0]));
			//System.out.println(xml2data);
			assertEquals(expected, xml2data);
		}
	}
	
	private static byte[] data2xml(String dataFileName, String copybookFileName) 
	throws FileNotFoundException, RecordException, IOException, JAXBException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		Cobol2GroupXml.newCb2Xml2Xml(copybookFileName)
					  .setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					  .setXmlMainElement("copybook")
					  .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	
	
	
	private static String xml2data(String dataFileName, String copybookFileName) 
	throws FileNotFoundException, RecordException, IOException, JAXBException, XMLStreamException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		Cobol2GroupXml.newCb2Xml2Xml(copybookFileName)
					  .setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					  .xml2Cobol(new FileInputStream(dataFileName), os);
		return new String(os.toByteArray());

	}
	
	private static String loadLocationFile() throws IOException {
		return Code.loadFile(Code.getFullName("compare/" + LOC_DOWNLOAD_TXT), "\r\n", true);
	}
}
