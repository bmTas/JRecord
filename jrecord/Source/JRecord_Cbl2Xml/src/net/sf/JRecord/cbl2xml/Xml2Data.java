/**
 * 
 */
package net.sf.JRecord.cbl2xml;

import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.impl.ConvertOptions;

/**
 * Convert a Xml file to a Cobol Data File using a Cobol Copybook
 * @author Bruce Martin
 *
 */
public class Xml2Data {

	/**
	 * @param args program arguments
	 * 
	 */
	public static void main(String[] args) throws RecordException, IOException, JAXBException, XMLStreamException {
		ConvertOptions opts = new ConvertOptions(args);
		
		if (opts.isOk()) {
			Cobol2Xml.newXmlConverter(opts)
					 .xml2Cobol(opts.inputFile, opts.outputFile);
		}
	}

}
