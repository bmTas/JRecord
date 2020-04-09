/**
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
      
package net.sf.JRecord.charIO;

import java.io.IOException;

/**
 * An ICharReader implementation read fixed length records
 * from a file as a String 
 * 
 * @author Bruce Martin
 *
 */
public class FixedLengthCharReader extends BaseCharReader {
	private final int length;
	private boolean eofPending = false;
	
	public FixedLengthCharReader(int length) {
		super();
		this.length = length;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#read()
	 */
	@Override
	public String read() throws IOException {
		if (eofPending) return null;

		char[] c = new char[length];

		int l = reader.read(c, 0, length);
		if (l < 0) {
			return null;
		}

		int read = l;
		while (read < length && (l = reader.read(c, read, length - read)) >= 0) {
			read += l;
		}
		eofPending = l < 0;
		if (read < length) {
			char[] tmp = new char[read];
			System.arraycopy(c, 0, tmp, 0, read);
			c = tmp;
		}
			
		return new String(c);
	}

}
