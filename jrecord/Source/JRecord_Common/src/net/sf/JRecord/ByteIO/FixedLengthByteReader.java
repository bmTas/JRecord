/*
 * Purpose: Record oriented reading of Fixed Length Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 */
package net.sf.JRecord.ByteIO;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;




/**
 * This class performs record oriented reading of Fixed Record Length
 * (i.e. every record has the same length) files into "Line's" [Array of Byte]
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class FixedLengthByteReader extends AbstractByteReader {

    private InputStream inStream;
	private BufferedInputStream stream = null;

	private int lineLength;



	/**
	 * This class provides record oriented reading of Fixed Length
	 * (ie every record is exactly the same length) Binary files.
	 *
	 * @param recordength length of the record
	 */
	public FixedLengthByteReader(int recordength) {
	    super();

	    lineLength = recordength;
	}


    /**
     * @see AbstractByteReader#open(java.io.InputStream)
     */
    public void open(InputStream inputStream) {

        inStream = inputStream;

        if (inputStream instanceof BufferedInputStream) {
        	stream = (BufferedInputStream) inputStream;
        } else {
        	stream = new BufferedInputStream(inputStream, BUFFER_SIZE);
        }
    }



    /**
     * @see AbstractByteReader#read()
     */
    public byte[] read()  throws IOException {
        byte[] ret = null;
        byte[] inBytes = new byte[lineLength];

        if (stream == null) {
            throw new IOException(AbstractByteReader.NOT_OPEN_MESSAGE);
        }

        if (readBuffer(stream, inBytes) > 0) {
            ret = inBytes;
        }

        return ret;
    }


    /**
     * @see AbstractByteReader#close()
     */
    public void close() throws IOException {

        inStream.close();
        stream = null;
    }

    /**
     * Set the Line Length of the file.
     * 
     * @param newLineLength The lineLength to set.
     */
    public void setLineLength(int newLineLength) {
        this.lineLength = newLineLength;
    }
}
