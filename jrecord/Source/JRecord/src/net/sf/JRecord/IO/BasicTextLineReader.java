package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.RecordException;
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
	  throws IOException, RecordException {

		  reader = r;
		  inStream = inputStream;
		  setLayout(layout);

		  reader.open(inputStream, font);
	  }

	/**
	 * @see net.sf.JRecord.IO.StandardLineReader#read()
	 */
	public AbstractLine read() throws IOException {
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
	 * @see net.sf.JRecord.IO.StandardLineReader#close()
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