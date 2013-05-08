/*
 * @Author Jean-Francois Gagnon
 * Created on 26/08/2005
 *
 * Purpose: writes "Line's" to a binary file
 */
package net.sf.JRecord.ByteIO;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;


/**
 * Writes Array of Bytes to a raw Fujitsu Cobol Variable Length file.
 * A Fujitsu Variable Length file is stored as
 * {RDW1}{Record1}{RDW1}{RDW2}{Record2}{RDW2}....
 *
 * where
 * <ul>
 * <li>{rwd} (record-descriptor} is a 4 byte net record length (Little Endian)
 * </ul>
 *
 * <p>Files written by this class contains record~descriptor~words.
 *
 * @author Jean-Francois Gagnon
 * 
 * v69.1 Jean-Francois Gagnon supplied fix for RDW calculation
 *  
 *
 */
public class FujitsuVbByteWriter extends AbstractByteWriter {

    private OutputStream outStream = null;
	//private ByteArrayOutputStream byteStream = new ByteArrayOutputStream();

    private byte[] rdw = new byte[4];



    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

        outStream = outputStream;
        //byteStream.reset();
    }


    /**
     * @see AbstractByteWriter#write(byte[])
     */
    public void write(byte[] rec) throws IOException {

        if (outStream == null) {
            throw new IOException(AbstractByteWriter.NOT_OPEN_MESSAGE);
        }

        updateRDW(rec.length , rdw);

        outStream.write(rdw);
        outStream.write(rec);
        outStream.write(rdw);

    }

    /**
     * Update a RDW (record descriptor record)
     *
     * @param length length to update
     * @param word word to be updated
     */
    private void updateRDW(int length, byte[] word) {
    	byte[] bytes = (BigInteger.valueOf(length)).toByteArray();

        // The value is stored in Little Endian format
        // toByteArray returns only non zero bytes in Big Endian order

       	for (int i = 3; i >= bytes.length; i--) {
            word[i] = 0;
        }

        for (int i = bytes.length - 1, j = 0; i >= 0; i--, j++) {
            word[i] = bytes[j];
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

