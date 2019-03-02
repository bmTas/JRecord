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
 * An ICharReader implementation using java's standard
 * Reader class to implement it 
 * 
 * @author Bruce Martin
 *
 */
public class StandardCharReader extends BaseCharReader {

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#read()
	 */
	@Override
	public String read() throws IOException {
		return reader.readLine();
	}

}
