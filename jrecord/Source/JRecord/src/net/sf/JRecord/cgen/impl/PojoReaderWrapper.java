package net.sf.JRecord.cgen.impl;

import java.io.IOException;

import net.sf.JRecord.cgen.def.IAsPojo;
import net.sf.JRecord.cgen.def.IReader;

public class PojoReaderWrapper<Line, InLine extends IAsPojo<Line>> implements IReader<Line> {

	private final IReader<InLine> reader;
	
	public PojoReaderWrapper(IReader<InLine> reader) {
		super();
		this.reader = reader;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IReader#read()
	 */
	@Override
	public Line read() throws IOException {
		InLine in = reader.read();
		if (in == null) {
			return null;
		}
		return in.asPojo();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IReader#close()
	 */
	@Override
	public void close() throws IOException {
		reader.close();
	}

}
