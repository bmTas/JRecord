/*
 * @author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose: Writing Record Orientated files
 */
package net.sf.JRecord.ByteIO;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;


/**
 * This abstract class is the base class for all <b>Byte~Writer</b>
 * classes
 *
 * @author Bruce Martin
 *
 */
public abstract class AbstractByteWriter {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";


    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName) throws IOException {
        open(new FileOutputStream(fileName));
    }


    /**
     * Open file for input
     *
     * @param outputStream input stream to write
     *
     * @throws IOException any IOerror
     */
    public abstract void open(OutputStream outputStream)
    throws IOException;


    /**
     * Read one line from the input file
     *
     * @param bytes line to write to the output file
     *
     * @throws IOException any IOerror
     */
    public abstract void write(byte[] bytes) throws IOException;


    /**
     * Closes the file
     *
     * @throws IOException any IOerror
     */
    public abstract void close() throws IOException;
}
