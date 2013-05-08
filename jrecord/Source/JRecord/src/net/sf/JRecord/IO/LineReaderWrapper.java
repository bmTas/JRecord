/*
 * @Author Bruce Martin
 * Created on 19/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;

/**
 * Creates a LineReader from a Byte-Reader (ByteIO package).
 * A Byte-Reader reads Lines from a File as a series of Bytes.
 * There a Byte Readers for <ol compact>
*   <li>Fixed Line Length files
 *   <li>Length Based lines
 * </ol>
 *
 * @author Bruce Martin
 *
 */
public class LineReaderWrapper extends AbstractLineReader {

    private AbstractByteReader reader;
    int i = 0;

    /**
     *  Create a LineReader from a Byte reader
     */
    public LineReaderWrapper(AbstractByteReader byteReader) {
        super();

        reader = byteReader;
    }

    /**
     * @param provider
     */
    public LineReaderWrapper(LineProvider provider, AbstractByteReader byteReader) {
        super(provider);

        reader = byteReader;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail pLayout)
            throws IOException, RecordException {

        reader.setLineLength(pLayout.getMaximumRecordLength());
        reader.open(inputStream);
        super.setLayout(pLayout);
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#read()
     */
    public AbstractLine read() throws IOException {
        byte bytes[] = reader.read();

        if (bytes == null) {
            return null;
        }
        return getLine(bytes);
    }

    protected byte[] rawRead() throws IOException {
    	return reader.read();
    }
    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#close()
     */
    public void close() throws IOException {
        reader.close();
    }

	/**
	 * @param reader the reader to set
	 */
	public final void setReader(AbstractByteReader reader) {
		this.reader = reader;
	}

}
