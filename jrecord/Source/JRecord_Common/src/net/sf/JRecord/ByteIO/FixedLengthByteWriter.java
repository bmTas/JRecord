/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose: writes "Line's" to a binary file
 */
package net.sf.JRecord.ByteIO;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;


/**
 *  writes an Array of bytes as a line in a fixed line length file (i.e. all lines are exactly the same length).
 *
 * @author Bruce Martin
 *
 */
public class FixedLengthByteWriter extends AbstractByteWriter {
    private OutputStream outStream = null;

    private final int recordLength, fillByte;
    
    
    public FixedLengthByteWriter(int recordLength) {
    	this.recordLength = recordLength;
    	fillByte = 0;
    }
    
    
    
//    /**
//     * create Fixed Length line writer
//     */
//    public FixedWidthByteWriter(int recordLength, String charset) {
//        this.recordLength = recordLength;
//        
//        byte[] bytes = Conversion.getBytes(" ", charset);
//        byte t = 0;
//        if (bytes != null && bytes.length == 1) {
//        	t = bytes[0];
//        }
//        fillByte = t;
//    }



    protected FixedLengthByteWriter(int recordLength, int fillByte) {
		super();
		this.recordLength = recordLength;
		this.fillByte = fillByte;
	}



	/**
     * @see net.sf.JRecord.ByteIO.FixedLengthByteWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

    	outStream = outputStream;
    	if (! (outputStream instanceof BufferedOutputStream)) {
     		outStream = new BufferedOutputStream(outputStream, 0x4000);
    	}
    }


    /**
     * @see net.sf.JRecord.ByteIO.FixedLengthByteWriter#write(byte[])
     */
    public void write(byte[] rec) throws IOException {

        if (outStream == null) {
            throw new IOException(AbstractByteWriter.NOT_OPEN_MESSAGE);
        }
        
        
		if (rec.length != recordLength ) {
			if (rec.length > recordLength) {
				outStream.write(rec, 0, recordLength);
			} else {
				outStream.write(rec);
				for (int i = recordLength - rec.length; i > 0; i--) {
					outStream.write(fillByte);
				}
			}
		} else {
			outStream.write(rec);
		}
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {

        outStream.close();
        outStream = null;
    }
}
