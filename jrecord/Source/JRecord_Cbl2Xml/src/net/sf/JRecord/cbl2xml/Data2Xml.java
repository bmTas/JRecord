/**
 * 
 */
package net.sf.JRecord.cbl2xml;

import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.def.Icb2xml2Xml;
import net.sf.JRecord.cbl2xml.impl.ConvertOptions;

/**
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
			Icb2xml2Xml cbl2xml;
			if (opts.useCobol) {
				cbl2xml = Cobol2Xml.newCobol2Xml(opts.cobolCopybook)
								   .setDialect(opts.dialect);
			} else {
				cbl2xml = Cobol2Xml.newCb2Xml2Xml(opts.cb2xmlCopybook);
			}
			cbl2xml
					.setFont(opts.font)
					.setFileOrganization(opts.fileOrganisation)
					.setDropCopybookNameFromFields(opts.dropCopybookName)
					.setXmlMainElement(opts.mainXmlTag)
				.cobol2xml(opts.inputFile, opts.outputFile);
		}
	}

}
