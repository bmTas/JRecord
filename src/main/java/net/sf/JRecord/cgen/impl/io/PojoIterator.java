package net.sf.JRecord.cgen.impl.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Iterator;

import net.sf.JRecord.cgen.def.IReader;

public class PojoIterator<ReadClass> implements Iterator<ReadClass> {

	private final IReader<ReadClass> lineReader;
	private ReadClass nextLine;
	private boolean endReached=false;

	
	
	PojoIterator(IReader<ReadClass> lineReader) throws IOException {
		super();
		this.lineReader = lineReader;
		this.nextLine = lineReader.read();
	}

	@Override
	public boolean hasNext() {
		return nextLine != null;
	}

	@Override
	public ReadClass next() {
		if (endReached) { return null; }
		
		ReadClass line = nextLine;
		
		try {
			if (nextLine == null) {
				endReached = true;
				lineReader.close();
			} else {		
				nextLine = lineReader.read();
			}
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		
		return line;
	}
	
}
