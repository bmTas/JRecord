package net.sf.JRecord.External.cb2xml;

import java.io.Reader;

import net.sf.cb2xml.analysis.Copybook;

public interface IReadCopybook {
	public Copybook getCopybook(
			Reader reader, String name,  int cobolDialect, boolean debug,
			int copybookFormat, int stackSize);

}
