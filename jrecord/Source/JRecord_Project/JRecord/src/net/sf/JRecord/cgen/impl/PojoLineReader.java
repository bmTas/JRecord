package net.sf.JRecord.cgen.impl;

import java.io.IOException;


import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.cgen.def.IReader;
import net.sf.JRecord.cgen.defJr.IToPojo;

/**
 * Read a java Pojo. I does this by reading a 
 * AbstractLine from a AbstractLineReader then converting it to a
 * Pojo using a IToPojo class
 *  
 * @author Bruce Martin
 *
 * @param <Pojo>
 */
public class PojoLineReader<Pojo> implements IReader<Pojo>{
	private final AbstractLineReader lineReader;
	private final IToPojo<Pojo> copy;
	protected AbstractLine nextLine;
	
	public PojoLineReader(AbstractLineReader lineReader, AbstractLine firstLine, IToPojo<Pojo> toPojo) throws IOException {

		this.lineReader = lineReader;

		this.nextLine = firstLine;
		this.copy = toPojo;
	}

	@Override
	public Pojo read() throws IOException {
		if (nextLine == null) {
			return null;
		}
		AbstractLine line = nextLine;
		nextLine = lineReader.read();
		
		return  copy.toPojo(line);
	}

	@Override
	public void close() throws IOException {
		lineReader.close();
	}
}
