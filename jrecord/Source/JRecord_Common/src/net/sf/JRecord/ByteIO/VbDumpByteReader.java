/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.ByteIO;

/**
 * ByteReader for Mainframe VB File that also include Block descriptor Word
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
 *       
 * @author Bruce Martin
 *
 */
public class VbDumpByteReader extends VbByteReader {

    /**
     * ByteReader for Mainframe VB File that also include Block descriptor Word
     */
    public VbDumpByteReader() {
        super(true);
    }
}
