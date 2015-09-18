package net.sf.JRecord.cbl2xml.zTest;

import java.io.IOException;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;


public class TstCblData2Xml {

    public static String getFullName(String filename) {
    	URL resource = TstCblData2Xml.class.getResource(filename);
    	if (resource == null) {
    		System.out.println(" --> Can not find: " + filename);
    	}
		return resource.getFile();
    }


	public static void main(String[] args) throws RecordException, IOException, JAXBException, XMLStreamException {
		ICobol2Xml cbl2xml = Cobol2GroupXml.newCobol2Xml(getFullName("cbl2xml_Test112.cbl"));
		cbl2xml.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		
		//cbl2xml.cobol2xml(getFullName("StoreSales5.txt"), "G:/Temp/Store_Sales_5.xml");
		
		//cbl2xml.xml2Cobol(getFullName("StoreSales5.xml"), "G:/Temp/Store_Sales_5.txt");

	}
}
