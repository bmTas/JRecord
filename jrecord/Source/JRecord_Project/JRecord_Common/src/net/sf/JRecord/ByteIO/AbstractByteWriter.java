
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
public abstract class AbstractByteWriter implements IByteRecordWriter {

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


    /* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.IByteRecordWriter#write(byte[])
	 */
    @Override
	public abstract void write(byte[] bytes) throws IOException;


    /* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.IByteRecordWriter#close()
	 */
    @Override
	public abstract void close() throws IOException;
}
