/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose: writes "Line's" to a binary file
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.ByteIO;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;


/**
 *  writes an Array of bytes as a line in a fixed line length file (i.e. all lines are exactly the same length).
 *
 * @author Bruce Martin
 *
 */
public class BinaryByteWriter extends AbstractByteWriter {
    private OutputStream outStream = null;
    private boolean addLength = false;

    private byte[] rdw = new byte[4];
    private byte[] eol = null;

    private int rdwAdjust = 4;

    /**
     * create binary line writer
     */
    public BinaryByteWriter() {
        super();
    }


//    /**
//     * create binary line writer
//     *
//     * @param includeRDW write RDW (record Descriptor Word) at the start
//     * of each record. The RDW consists of
//     * <ul Compact>
//     *   <li>2 byte length (big endian format)
//     *   <li>2 bytes (hex zeros)
//     * </ul>
//     */
//    public FixedLengthByteWriter(final boolean includeRDW) {
//    	this(includeRDW, true, null);
//    }

    /**
     * create binary line writer
     *
     * @param includeRDW write RDW (record Descriptor Word) at the start
     * of each record. The RDW consists of
     * <ul Compact>
     *   <li>2 byte length (big endian format)
     *   <li>2 bytes (hex zeros)
     * </ul>
     * @param addRdwToLength wether to add 4 bytes to RDW length
     */
    public BinaryByteWriter(final boolean includeRDW, boolean addRdwToLength, byte[] eolByte) {
        super();
        addLength = includeRDW;
        eol = eolByte;
        rdw[2]    = 0;
        rdw[3]    = 0;

        if (! addRdwToLength) {
        	rdwAdjust = 0;
        }
    }


    /**
     * @see net.sf.JRecord.ByteIO.BinaryByteWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

    	outStream = outputStream;
    	if (! (outputStream instanceof BufferedOutputStream)) {
     		outStream = new BufferedOutputStream(outputStream, 0x4000);
    	}
    }


    /**
     * @see net.sf.JRecord.ByteIO.BinaryByteWriter#write(byte[])
     */
    public void write(byte[] line) throws IOException {

        if (outStream == null) {
            throw new IOException(AbstractByteWriter.NOT_OPEN_MESSAGE);
        }
        byte[] rec = line;

        if (addLength) {
            byte[] bytes = (BigInteger.valueOf(rec.length + rdwAdjust)).toByteArray();

            rdw[1] = bytes[bytes.length - 1];
            rdw[0] = 0;
            if (bytes.length > 1) {
                rdw[0] = bytes[bytes.length - 2];
            }
            outStream.write(rdw);
        }

        outStream.write(rec);

        if (eol != null) {
        	outStream.write(eol);
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
