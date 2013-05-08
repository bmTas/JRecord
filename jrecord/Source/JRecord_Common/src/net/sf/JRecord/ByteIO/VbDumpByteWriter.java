/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose: writes "Line's" to a binary file
 */
package net.sf.JRecord.ByteIO;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;



/**
 * Writes "Line's" to a raw Mainframe-VB Dump file.
 * A VB (variable blocked) file is stored as
 * {Block-Descriptor-Word}{RDW}{Record}{rwd}{Record}....
 *
 * where
 * <ul>
 * {Block-Descriptor-Word} is either
 * <ul compact>
 *   <LI>first bit 0, next 15 bits are the block length last 2 bytes hex zero
 *   <li>first bit 1 followed by 31 bit length
 * </ul>
 *
 * <li>{rwd} (record-descriptor} is a 2 byte length followed by 2 bytes hex
 * zero.
 * </ul>
 *
 * <p>Files written by this class contains both  block~descriptor~words
 * and record~descriptor~words. Of course block~descriptor~words make
 * no sense on Windows / Unix and these files can never be moved back to
 * the Mainframe.
 * 
 * i.e. The file consists of
 * 
 *   [Block Descriptor - 4 bytes containing the 2 byte block Length]
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *           ................................
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *   [Block Descriptor - 4 bytes containing the 2 byte block Length]
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *           ................................
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *     ..............
  * 
 *
 * @author Bruce Martin
 *
 */
public class VbDumpByteWriter extends AbstractByteWriter {

	private static final int DEFAULT_BUFFER_SIZE = 27998;

    private OutputStream outStream = null;
	private ByteArrayOutputStream byteStream = new ByteArrayOutputStream();

	private int blockSize;

    private byte[] rdw = new byte[4];


    /**
     * create binary line writer for VB files (including the
     * Block-Descriptor-Word).
     */
    public VbDumpByteWriter() {
        this(DEFAULT_BUFFER_SIZE);
    }


    /**
     * create binary line writer (including the
     * Block-Descriptor-Word).
     *
     * @param pBlockSize block size of the file
     */
    public VbDumpByteWriter(final int pBlockSize) {
        super();

        blockSize = pBlockSize;
        rdw[2]    = 0;
        rdw[3]    = 0;
    }


    /**
     * @see AbstractByteWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

        outStream = outputStream;
        byteStream.reset();
    }


    /**
     * @see AbstractByteWriter#write(byte[])
     */
    public void write(byte[] rec) throws IOException {

        if (outStream == null) {
            throw new IOException(AbstractByteWriter.NOT_OPEN_MESSAGE);
        }
        //byte[] rec = line.getData();

        if (byteStream.size() + rec.length + 4 >= blockSize) {
            writeBlock();
        }

        updateRDW(rec.length + 4, rdw);
        byteStream.write(rdw);


        byteStream.write(rec);
    }


    /**
     * writes a block to the output file
     *
     * @throws IOException any IO error
     */
    private void writeBlock() throws IOException {

    	updateRDW(byteStream.size() + 4, rdw);
    	outStream.write(rdw);
    	byteStream.writeTo(outStream);
    	byteStream.reset();
    }

    /**
     * Update a RDW (record descriptor record) / BDW (Block descriptor
     * Word)
     *
     * @param length length to update
     * @param word word to be updated
     */
    private void updateRDW(int length, byte[] word) {
    	byte[] bytes = (BigInteger.valueOf(length)).toByteArray();

        word[1] = bytes[bytes.length - 1];
        word[0] = 0;
        if (bytes.length > 1) {
            word[0] = bytes[bytes.length - 2];
        }
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {

        if (byteStream.size() > 0) {
            writeBlock();
        }

        outStream.close();
        outStream = null;
    }
}
