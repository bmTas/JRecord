package net.sf.JRecord.External;

import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.Log.AbsSSLogger;

import org.xml.sax.SAXException;

public class XmlFileLoader implements CopybookLoader {

	/**
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public ExternalRecord loadCopyBook(String copyBookFile, int splitCopybookOption, int dbIdx, String font, int binFormat, int systemId, AbsSSLogger log) 
	throws IOException, SAXException, ParserConfigurationException, RecordException {
		int i;
		XmlLineReader r = new XmlLineReader(true);
		r.open(copyBookFile);
		
		for (i = 0; i < 20000 && (r.read() != null); i++) {
		}
		
		r.close();
		
		return ToExternalRecord.getInstance()
				.getExternalRecord(r.getLayout(), Conversion.getCopyBookId(copyBookFile), systemId);
	}

}
