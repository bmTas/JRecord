/**
 * 
 */
package net.sf.JRecord.cbl2xml;

import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.ConvertOptions;

/**
 * @author Bruce01
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
			ICobol2Xml cbl2xml;
			if (opts.useCobol) {
				cbl2xml = Cobol2Xml.newCobol2Xml(opts.cobolCopybook);
			} else {
				cbl2xml = Cobol2Xml.newCb2Xml2Xml(opts.cobolCopybook);
			}
			cbl2xml
					.setFont(opts.font)
					.setFileOrganization(opts.fileOrganisation)
					.setDialect(opts.dialect)
					.setDropCopybookNameFromFields(opts.dropCopybookName)
					.setXmlMainElement(opts.mainXmlTag)
				.xml2Cobol(opts.inputFile, opts.outputFile);
		}
	}

}
