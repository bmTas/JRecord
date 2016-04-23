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

import net.sf.JRecord.Common.Constants;

/**
 * This class will write an Array of Bytes to a file then write the appropriate End-Of-Line characters to the file.
 * This allows hex field seperators to be used.
 *
 * @author Bruce Martin
 * @version 0.68
 */
public class ByteTextWriter extends BinaryByteWriter {

	/**
	 * Standard Text file Byte writer using System End-of-Line
	 */
	public ByteTextWriter() {
		super(false, false, Constants.SYSTEM_EOL_BYTES);
	}

	/**
	 * Standard Text file Byte writer using user supplied End-of-Line
	 * @param eol end of line
	 */
	public ByteTextWriter(String eol) {
		super(false, false, eol.getBytes());
	}

	/**
	 * Standard Text file Byte writer using user supplied End-of-Line
	 * @param eol end of line
	 */
	public ByteTextWriter(byte[] eol) {
		super(false, false, eol);
	}
}
