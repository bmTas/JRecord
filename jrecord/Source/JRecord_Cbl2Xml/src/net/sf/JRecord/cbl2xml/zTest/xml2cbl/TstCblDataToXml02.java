package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.assertArrayEquals;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;

import org.junit.Test;
import org.xml.sax.SAXException;

public class TstCblDataToXml02 {

	private String[][] files ={
			{"DTAR020.cbl",  "DTAR020.bin",  "DTAR020_DataN.xml", "N", "F"},
			{"DTAR107.cbl",  "DTAR107.bin",  "DTAR107_DataN.xml", "N", "F"},
			{"DTAR192.cbl",  "DTAR192.bin",  "DTAR192_DataN.xml", "N", "F"},
			{"DTAR1000.cbl", "DTAR1000.bin", "DTAR1000_DataN.xml","N", "V"},
			{"DTAR020.cbl",  "DTAR020.bin",  "DTAR020_DataY.xml", "Y", "F"},
			{"DTAR107.cbl",  "DTAR107.bin",  "DTAR107_DataY.xml", "Y", "F"},
			{"DTAR192.cbl",  "DTAR192.bin",  "DTAR192_DataY.xml", "Y", "F"},
			{"DTAR1000.cbl", "DTAR1000.bin", "DTAR1000_DataY.xml","Y", "V"},
	};


	//private String firstLine = "TAR5839DCDC - Taras Ave                                                             30-68 Taras Ave                         Altona North                       3025      VICA";
	@Test
	public void testData2Xml() throws IOException, SAXException, ParserConfigurationException, RecordException, JAXBException, XMLStreamException {
		for (String[] d : files) {
			check(d);
		}
	}
	
	public void testData2Xml_DTAR107() throws IOException, SAXException, ParserConfigurationException, RecordException, JAXBException, XMLStreamException {
		check(files[4]);
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
	private void check(String[] d) throws FileNotFoundException,
			RecordException, IOException, JAXBException, XMLStreamException,
			SAXException, ParserConfigurationException,
			FactoryConfigurationError {
		String copybookName;
		String dataName;
		String xmlDataName;
		
		copybookName = Code.getFullName("cobol/" + d[0]);
		dataName = Code.getFullName(d[1]);
		
		byte[] doc = data2xml(dataName, copybookName, "Y".equalsIgnoreCase(d[3]),  "V".equalsIgnoreCase(d[4]));
		
		
		//System.out.println(XmlUtils.domToString(doc));
		System.out.println("Copybook: " + d[0] + " " + d[2]);
		System.out.println();
		System.out.println(new String(doc));
		
		xmlDataName = Code.getFullName("xml/" + d[2]);
		Code.compare("File: " + copybookName,  xmlDataName, doc);
	} 
	
	private static byte[] data2xml(String dataFileName, String copybookFileName, boolean dropCopybookName, boolean vb) 
	throws FileNotFoundException, RecordException, IOException, JAXBException, XMLStreamException, SAXException, ParserConfigurationException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		int fileOrg = Constants.IO_FIXED_LENGTH;
		if (vb) {
			fileOrg = Constants.IO_VB;
		}
		
		Cobol2GroupXml.newCobol2Xml(copybookFileName)
					  .setFileOrganization(fileOrg)
					  .setXmlMainElement("copybook")
					  .setFont("cp037")
					  .setDialect(ICopybookDialects.FMT_MAINFRAME) // This is the default
					  .setDropCopybookNameFromFields(dropCopybookName)
					  .cobol2xml(new FileInputStream(dataFileName), os);

	    return os.toByteArray();
	}
	
	
	@Test
	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException, RecordException, JAXBException, XMLStreamException {
		byte[] xml2data;
		byte[][] expected = new byte[4][];
		String[] d = files[0];
		
		for (int i = 0; i < 4; i++) {
			expected[i] = readFile(Code.getFullName(files[i][1]));
		}
		for (int i = 0; i < files.length; i++) { // xml2data only works when there are no arrays !!!
			try {
				d = files[i];
				System.out.println("->> " + d[0] + " " + d[2]);
				xml2data = xml2data(
						Code.getFullName("xml/" + d[2]), 
						Code.getFullName("cobol/" + d[0]), 
						"Y".equalsIgnoreCase(d[3]),  "V".equalsIgnoreCase(d[4]));
				//System.out.println(xml2data);
				assertArrayEquals("idx=" + 1, expected[i%4], xml2data);
			} catch (Exception e) {
				System.err.println();
				System.err.println("   --> " + i + " " + d[0]  + " " + d[1] + " " + d[2]);
				System.err.println();
			}
		}
	}

	private static byte[] xml2data(String dataFileName, String copybookFileName, boolean dropCopybookName, boolean vb) 
	throws FileNotFoundException, RecordException, IOException, JAXBException, XMLStreamException {
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(0x10000);
		int fileOrg = Constants.IO_FIXED_LENGTH;
		if (vb) {
			fileOrg = Constants.IO_VB;
		}

		Cobol2GroupXml.newCobol2Xml(copybookFileName)
					  .setFileOrganization(fileOrg)
					  .setXmlMainElement("copybook")
					  .setFont("cp037")
					  .setDropCopybookNameFromFields(dropCopybookName)
					  .xml2Cobol(new FileInputStream(dataFileName), os);
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
