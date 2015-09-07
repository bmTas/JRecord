/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose: writes "Line's" to a binary file
 */
package net.sf.JRecord.IO;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;


/**
 *  writes "Line's" to a binary file
 *
 * @author Bruce Martin
 *
 */
public class FixedLengthWriter extends AbstractLineWriter {
    private OutputStream outStream = null;

    private LayoutDetail layout = null;
    private int recLength;
    private byte fillByte = 0;


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

        outStream = outputStream;
    	if (! (outputStream instanceof BufferedOutputStream)) {
     		outStream = new BufferedOutputStream(outputStream, 0x4000);
    	}
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#write(net.sf.JRecord.Details.AbstractLine)
     */
    public void write(AbstractLine line) throws IOException {

        if (outStream == null) {
            throw new IOException(AbstractLineWriter.NOT_OPEN_MESSAGE);
        }
        byte[] rec = line.getData();

        if (layout == null) {
        	layout = line.getLayout();
        	recLength = layout.getMaximumRecordLength();
        	if (! layout.isBinary()) {
        		fillByte = layout.getSpaceByte();
        	}
        }
		if (rec.length != recLength ) {
			if (rec.length > recLength) {
				outStream.write(rec, 0, recLength);
			} else {
				outStream.write(rec);
				for (int i = recLength - rec.length; i > 0; i--) {
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
