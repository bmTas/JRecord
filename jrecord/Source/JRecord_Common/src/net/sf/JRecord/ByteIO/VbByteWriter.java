/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.ByteIO;

/**
 * Write Array of bytes to either a Mainframe VB file or a Open Cobol VB file. A VB file consists of
 * 
 * {RDW 1}Data for the Line 1{RDW 1}Data for the Line...
 *
 *
 * Where RDW (Record Descriptor Word) is
 * 
 *  2 Byte Record Length (Big Endian)
 *  2 Bytes - hex zero.
 *
 * <b>Note:</b> Mainframe RDW includes the RDW length (4 bytes) open cobol's does not 
 * 
 * @author Bruce Martin
 *
 */
public class VbByteWriter extends FixedLengthByteWriter {

    /**
     *
     */
    public VbByteWriter() {
        super(true, true, null);
    }
    
    public VbByteWriter(boolean addRdwToLength) {
    	super(true, addRdwToLength, null);
    }
}
