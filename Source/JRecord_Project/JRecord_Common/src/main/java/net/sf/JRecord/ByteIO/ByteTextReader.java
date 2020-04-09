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
import java.util.ArrayList;
import java.util.List;


/**
 * Reads a standard Text file (like standard readLine of Class BufferedReader) except it will return an
 * Array of Bytes (instead of a String). This allows binary data to be in a line (i.e. using X'FF' as a field
 * seperator). It has a limit of 256kb on the line size.
 *
 * @author  Bruce Martin
 * @version 0.68
 */
public class ByteTextReader extends BaseByteTextReader {

	public ByteTextReader() {
		super();
	}

	public ByteTextReader(String charSet) {
		super(charSet);
	}

	public static List<byte[]> readStream(String charSet, InputStream in, int maxNoLines) throws IOException {
		ArrayList<byte[]> ret = new ArrayList<byte[]>();
		byte[] b;
		boolean all = maxNoLines < 0;
		ByteTextReader r = new ByteTextReader(charSet);

		r.open(in);

		while ((all || ret.size() < maxNoLines)
			&& (b = r.read()) != null) {
			ret.add(b);
		}

		r.close();

		return ret;
	}
}
