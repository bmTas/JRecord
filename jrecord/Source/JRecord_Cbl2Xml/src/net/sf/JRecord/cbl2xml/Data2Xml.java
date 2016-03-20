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
 * Convert Cobol Data File to Xml using a Cobol copybook
 * 
 * @author Bruce Martin
 *
 */
public class Data2Xml {

	/**
	 * @param args program arguments
	 * 
	 */
	public static void main(String[] args) throws RecordException, IOException, JAXBException, XMLStreamException {
		ConvertOptions opts = new ConvertOptions(args);
		
		if (opts.isOk()) {
			Cobol2Xml.newXmlConverter(opts)
					 .cobol2xml(opts.inputFile, opts.outputFile);
		}
	}
}
