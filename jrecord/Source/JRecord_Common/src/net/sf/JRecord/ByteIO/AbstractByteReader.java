/*
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose:  reading Record Orientated files
 */
package net.sf.JRecord.ByteIO;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;


/**
 * This abstract class is the base class for all <b>Byte~Reader</b>
 * classes
 *
 * @author Bruce Martin
 *
 */
public abstract class AbstractByteReader {


    public static final String NOT_OPEN_MESSAGE = "File has not been opened";
	public static final int BUFFER_SIZE = 16384;

    private long bytesRead = 0;

	/**
	 * create Binary Line Reader
	 */
	public AbstractByteReader() {
	    super();
	}




    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName) throws IOException {
        open(new FileInputStream(fileName));
    }


    /**
     * Open file for input
     *
     * @param inputStream input stream to be read
     *
     * @throws IOException any IOerror
     */
    public abstract void open(InputStream inputStream)
    throws IOException;



    /**
     * Read one line from the input file
     *
     * @return line read in
     *
     * @throws IOException io error
     */
    public abstract byte[] read() throws IOException;


    /**
     * Closes the file
     *
     * @throws IOException io error
     */
    public abstract void close() throws IOException;



	/**
	 * Read a complete buffers worth of data into buf from a input stream.
	 *
	 * @param in stream to be read.
	 * @param buf buffer to be loaded with data
	 *
	 * @return the number of bytes read
	 * @throws IOException IO Exception
	 */
	protected final int readBuffer(final InputStream in,
	        					   final byte[] buf)
				throws IOException {
	    int num = in.read(buf);
	    int total = num;

	    while (num >= 0 && total < buf.length) {
	        num = in.read(buf, total, buf.length - total);
	        total += Math.max(0, num);
	    }

	    bytesRead += total;
	    
	    return total;
	}

    /**
     * @param lineLength The lineLength to set.
     */
    public void setLineLength(int lineLength) {
    }

    
    public boolean canWrite() {
    	return true;
    }


	public long getBytesRead() {
		return bytesRead;
	}
}
