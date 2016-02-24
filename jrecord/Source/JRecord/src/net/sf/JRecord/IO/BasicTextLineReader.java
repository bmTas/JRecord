package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.charIO.ICharReader;

public abstract class BasicTextLineReader extends AbstractLineReader {

	private InputStream inStream = null;
	private ICharReader reader;

	public BasicTextLineReader() {
		super();
	}

	public BasicTextLineReader(LineProvider provider) {
		super(provider);
	}

	  protected void open(ICharReader r, InputStream inputStream, LayoutDetail layout, String font)
	  throws IOException {

		  reader = r;
		  inStream = inputStream;
		  setLayout(layout);

		  reader.open(inputStream, font);
	  }

	/**
	 * Method to do the actual read. It is called by read in AbstractLineReader
	 * @see AbstractLineReader#read()
	 */
	@Override
	public AbstractLine readImplementation() throws IOException {
	    AbstractLine ret = null;
	
	    if (reader == null) {
	        throw new IOException(AbstractLineReader.NOT_OPEN_MESSAGE);
	    }
	    String s = reader.read();
	
	    if (s != null) {
	        ret = getLine(s);
	    }
	
	    return ret;
	}

	/**
	 * @see net.sf.JRecord.JRecordInterface1#close()
	 */
	public void close() throws IOException {
	
	    reader.close();
	    if (inStream != null) {
			inStream.close();
	    }
	
		inStream  = null;
	}

	public final ICharReader getReader() {
		return reader;
	}

}