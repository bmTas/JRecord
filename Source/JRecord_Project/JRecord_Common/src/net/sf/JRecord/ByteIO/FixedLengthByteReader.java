/*
 * Purpose: Record oriented reading of Fixed Length Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;




/**
 * This class performs record oriented reading of Fixed Record Length
 * (i.e. every record has the same length) files into "Line's" [Array of Byte]
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class FixedLengthByteReader extends AbstractByteReader {

    private InputStream inStream;
	private BufferedInputStream stream = null;

	private int lineLength;


	/**
	 * This class provides record oriented reading of Fixed Length
	 * (ie every record is exactly the same length) Binary files.
	 *
	 * @param recordength length of the record
	 */
	public FixedLengthByteReader(int recordength) {
	    super();

	    lineLength = recordength;
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
        byte[] inBytes = new byte[lineLength];

        if (stream == null) {
            throw new IOException(AbstractByteReader.NOT_OPEN_MESSAGE);
        }

        if (readBuffer(stream, inBytes) > 0) {
            ret = inBytes;
        }

        return ret;
    }


    /**
     * @see AbstractByteReader#close()
     */
    public void close() throws IOException {

    	if (inStream != null) {
    		inStream.close();
    	}
        stream = null;
    }

    /**
     * Set the Line Length of the file.
     * 
     * @param newLineLength The lineLength to set.
     */
    public void setLineLength(int newLineLength) {
        this.lineLength = newLineLength;
    }
}
