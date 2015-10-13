package net.sf.JRecord.cbl2xml.zTest;

import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.Data2Xml;

public class Ttt {

	public static void main(String[] args) throws RecordException, IOException, JAXBException, XMLStreamException {
		String[] a = {
				"-cobol", "DTAR020.cbl", "-font", "cp037", 
				"-fileOrganisation", "FixedWidth", "-input", "DTAR020.bin",
				"-output", "out/DTAR020_A.xml"
		};
 
		Data2Xml.main(a);
	}

}
