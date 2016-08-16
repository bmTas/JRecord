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

import java.io.IOException;
import java.io.InputStream;

public interface IByteReader {

	/**
	 * Open file for input
	 *
	 * @param fileName filename to be opened
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(String fileName) throws IOException;

	/**
	 * Open file for input
	 *
	 * @param inputStream input stream to be read
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(InputStream inputStream) throws IOException;

	/**
	 * Read one line from the input file as an array of bytes
	 *
	 * @return line read in
	 *
	 * @throws IOException io error
	 */
	public abstract byte[] read() throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException io error
	 */
	public abstract void close() throws IOException;

	/**
	 * @param lineLength The lineLength to set.
	 */
	public abstract void setLineLength(int lineLength);

	public abstract boolean canWrite();

	public abstract long getBytesRead();

}