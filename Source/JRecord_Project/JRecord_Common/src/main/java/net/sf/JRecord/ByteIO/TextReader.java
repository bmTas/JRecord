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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import net.sf.JRecord.Common.Conversion;

public class TextReader extends AbstractByteReader {

	private InputStream inStream;
	private InputStreamReader stdReader;
	private String font;
	private BufferedReader reader = null;
	
	
	/**
	 * @param font
	 */
	public TextReader(String font) {
		this.font = font;
	}


	@Override
	public void open(InputStream inputStream) throws IOException {
       inStream = inputStream;

		if (font == null || "".equals(font)) {
		    stdReader = new InputStreamReader(inputStream);
		} else {
		    try {
		        stdReader = new InputStreamReader(inputStream, font);
		    } catch (Exception e) {
 		        stdReader = new InputStreamReader(inputStream);
		    }
		}
		reader = new BufferedReader(stdReader);

	}

	@Override
	public byte[] read() throws IOException {
		return Conversion.getBytes(reader.readLine(), font);
	}

	@Override
	public void close() throws IOException {
		stdReader.close();
		inStream.close();
	}

}
