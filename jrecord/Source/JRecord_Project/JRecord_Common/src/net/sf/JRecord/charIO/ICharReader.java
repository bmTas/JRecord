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
      
package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.InputStream;

/**
 * Defines a class that will read a logical record from a file
 * as a String
 * 
 * @author Bruce Martin
 *
 */
public interface ICharReader {

	/**
	 * Open file for input
	 *
	 * @param fileName filename to be opened
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(String fileName, String font) throws IOException;

	/**
	 * Open file for input
	 *
	 * @param inputStream input stream to be read
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(InputStream inputStream, String font) throws IOException;

	/**
	 * Read one line from the input file
	 *
	 * @return line read in
	 *
	 * @throws IOException io error
	 */
	public abstract String read() throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException io error
	 */
	public abstract void close() throws IOException;
}