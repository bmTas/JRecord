package net.sf.JRecord.External.cb2xml;

import java.io.Reader;

import net.sf.cb2xml.analysis.Copybook;

public class CobolCopybookReader implements IReadCopybook {
	@Override
	public Copybook getCopybook(Reader reader, String name, int cobolDialect, boolean debug, int copybookFormat,
			int stackSize) {
		return net.sf.JRecord.External.Def.Cb2Xml
				.getCopybook(reader, name, cobolDialect, false, copybookFormat, stackSize);
	}
}
