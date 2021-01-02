package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;

/**
 * C
 * @author Bruce Martin
 *
 */
public class SingleLineReader extends AbstractLineReader {

	private AbstractLine line;
	
	public SingleLineReader(AbstractLine line) {
		setLine(line);
	}

	public void setLine(AbstractLine line) {
		this.line = line;
		super.setLayout(line.getLayout());
	}

	@Override
	public void open(InputStream inputStream, LayoutDetail pLayout) throws IOException {
		
	}

	@Override
	public AbstractLine readImplementation() throws IOException {
		AbstractLine ret = line;
		line = null;
		return ret;
	}

	@Override
	public void close() throws IOException {
		line = null;
	}

}
