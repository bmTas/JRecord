
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
 *    Copyright (c) 26/08/2005, Bruce Martin, All Rights Reserved.
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
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose:  reading Record Orientated files
 */
package net.sf.JRecord.ByteIO;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;


/**
 * This abstract class is the base class for all <b>Byte~Reader</b>
 * classes
 *
 * @author Bruce Martin
 *
 */
public abstract class AbstractByteReader implements IByteReader {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";
	public static final int BUFFER_SIZE = 16384;

    private long bytesRead = 0;

	/**
	 * create Binary Line Reader
	 */
	public AbstractByteReader() {
	    super();
	}




    /* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.AbstractBByteReader#open(java.lang.String)
	 */
    @Override
	public void open(String fileName) throws IOException {
        open(new FileInputStream(fileName));
    }





	/**
	 * Read a complete buffers worth of data into buf from a input stream.
	 *
	 * @param in stream to be read.
	 * @param buf buffer to be loaded with data
	 *
	 * @return the number of bytes read
	 * @throws IOException IO Exception
	 */
	protected final int readBuffer(final InputStream in,
	        					   final byte[] buf)
				throws IOException {
		return readBuffer(in, buf, 0);
//	    int num = in.read(buf);
//	    int total = num;
//
//	    while (num >= 0 && total < buf.length) {
//	        num = in.read(buf, total, buf.length - total);
//	        total += Math.max(0, num);
//	    }
//
//	    bytesRead += total;
//
//	    return total;
	}


	protected final int readBuffer(InputStream in, final byte[] buf, int inTotal)
	throws IOException {

		int total = inTotal;
		int num = in.read(buf, total, buf.length - total);

		while (num >= 0 && total + num < buf.length) {
			total += num;
			num = in.read(buf, total, buf.length - total);
		}

		if (num > 0) {
			total += num;
		}

		incBytesRead(total - inTotal);

		return total;
	}

    /* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.AbstractBByteReader#setLineLength(int)
	 */
    @Override
	public void setLineLength(int lineLength) {
    }


    /* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.AbstractBByteReader#canWrite()
	 */
    @Override
	public boolean canWrite() {
    	return true;
    }


	/* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.AbstractBByteReader#getBytesRead()
	 */
	@Override
	public long getBytesRead() {
		return bytesRead;
	}

	protected final void incBytesRead(long amount) {
		bytesRead += amount;
	}
}
