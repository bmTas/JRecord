package net.sf.JRecord.cbl2xml;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;

public class Cobol2Xml {

	public static ICobol2Xml newCobol2Xml(String cobolCopybook) {
		return Cobol2GroupXml.newCobol2Xml(cobolCopybook);
	}
	
	
	public static ICobol2Xml newCobol2Xml(InputStream cobolCopybook, String copybookName) throws IOException {
		return Cobol2GroupXml.newCobol2Xml(cobolCopybook, copybookName);
	}

	
	public static ICobol2Xml newCb2Xml2Xml(String cobolCopybook) {
		return Cobol2GroupXml.newCb2Xml2Xml(cobolCopybook);
	}
	
	
	public static ICobol2Xml newCb2Xml2Xml(InputStream cobolCopybook, String copybookName) throws IOException {
		return Cobol2GroupXml.newCb2Xml2Xml(cobolCopybook, copybookName);
	}

}
