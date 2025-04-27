package net.sf.JRecord.IO;

import java.io.Closeable;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Iterator;

import net.sf.JRecord.Details.AbstractLine;

/**
 * 
 * @author Bruce Martin
 *
 */
public class ReadLineIterator implements Iterator<AbstractLine> {

	private final IReadLine lineReader;
	private AbstractLine nextLine;
	private boolean endReached=false;
	
	
	public ReadLineIterator(IReadLine lineReader) throws IOException {
		super();
		this.lineReader = lineReader;
		this.nextLine = lineReader.read();
	}

	@Override
	public boolean hasNext() {
		return nextLine != null;
	}

	@Override
	public AbstractLine next() {
		if (endReached) { return null; }
		
		AbstractLine line = nextLine;
		
		try {
			if (nextLine == null) {
				endReached = true;
				/*if (lineReader instanceof AbstractLineReader ) {
					((AbstractLineReader) lineReader).close();
				} else*/ if (lineReader instanceof Closeable ) {
					((Closeable) lineReader).close();
				}
			} else {		
				nextLine = lineReader.read();
			}
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		
		return line;
	}

}
