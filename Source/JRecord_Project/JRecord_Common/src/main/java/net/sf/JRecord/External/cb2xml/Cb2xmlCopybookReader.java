package net.sf.JRecord.External.cb2xml;

import java.io.Reader;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.util.Cb2xmlReader;

public class Cb2xmlCopybookReader implements IReadCopybook {
	@Override
	public Copybook getCopybook(Reader reader, String name, int cobolDialect, boolean debug, int copybookFormat,
			int stackSize) {
		try {
			return (new Cb2xmlReader()).parseCobolCopybook(reader);
		} catch (XMLStreamException e) {
			throw new RuntimeException("Error creating XmlStream", e);
		}
	}
}
