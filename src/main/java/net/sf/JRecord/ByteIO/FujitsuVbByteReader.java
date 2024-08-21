
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Jean-Francois Gagnon
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 26/06/2006, Jean-Francois Gagnon/Bruce Martin, All Rights Reserved.
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
      
/*
 * Purpose: Record oriented reading of Variable Record Length Fujitsu files
 *
 * @Author Jean-Francois Gagnon
 * Created on 26/06/2006
 *
 */
package net.sf.JRecord.ByteIO;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;



/**
 * This class performs record oriented reading of:
 * </ul compact>
 *   <li>Fujitsu Cobol Variable Record Length Binary files
 * </ul>
 *
 * into an Array of bytes
 *
 * @author Jean-Francois Gagnon
 * 0.69.1  Bruce Martin Fix to support lines up to 64000 bytes long
 */
public class FujitsuVbByteReader extends AbstractByteReader {

    private InputStream inStream;
	private BufferedInputStream stream = null;

	private int lineNumber = 0;

	/**
	 * record descriptor word, it consists of
	 * 2 bytes length
	 * 2 bytes (hex zero)
	 */
	private byte[] rdw = new byte[4];
	private byte[] rdwLength = new byte[4];

	/**
	 * This class provides record oriented reading of Variable
	 * Record Length Binary files where the record length is held in
	 * 4 byte Record-Descriptor-Word at the start of the record.
	 */
	public FujitsuVbByteReader() {
	    super();
	    
	    rdwLength[0] = 0;
	    rdwLength[1] = 0;
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

        if (stream == null) {
            throw new IOException(AbstractByteReader.NOT_OPEN_MESSAGE);
        }

        lineNumber += 1;
        if (readBuffer(stream, rdw) > 0) {
            if (rdw[2] != 0 || rdw[3] != 0) {
                throw new IOException(
                          "Invalid Record Descriptor word at line "
                        + lineNumber
                      );
            }

            rdwLength[2] = rdw[1];
            rdwLength[3] = rdw[0];

        	int lineLength = (new BigInteger(rdwLength)).intValue();
            byte[] inBytes = new byte[lineLength];

            if (readBuffer(stream, inBytes) > 0) {
                ret = inBytes;
                // Read RDW at end of record
                if (readBuffer(stream, rdw) < rdw.length) {
                	throw new IOException("Line missing End of line length");
                }
			}
        }

        return ret;
    }

    /**
     * @see AbstractByteReader#close()
     */
    public void close() throws IOException {

        inStream.close();
        stream.close();
        stream = null;
    }

}

