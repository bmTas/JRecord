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

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Common.Conversion;

public class MicroFocusByteWriter extends AbstractByteWriter {

	private MicroFocusFileHeader header = null;
	private OutputStream outStream = null;

	private byte[] len;
	

	@Override
	public void open(OutputStream outputStream) throws IOException {
        outStream = new BufferedOutputStream(outputStream, 8192);
	}
	
	public void writeHeader(MicroFocusFileHeader fileHeader)  throws IOException {
		header = fileHeader;
		outStream.write(header.getHeaderRec());
		
		if (header.getMaxLength() < MicroFocusFileHeader.MIN_4_BYTE_LENGTH) {
			len = new byte[2];
		} else {
			len = new byte[4];
		}

	}

	@Override
	public void write(byte[] bytes) throws IOException {

		if (header == null) {
			throw new IOException("Microfocus Header has not been written");
		}
		
		if (bytes.length > header.getMaxLength()) {
			throw new IOException("Record Length is greater than that specified");
		}
		int recLengthbytes= header.getMaxLength() < MicroFocusFileHeader.MIN_4_BYTE_LENGTH
				? 2 
				: 4 ;
		try {
			Conversion.setLong(len, 0, recLengthbytes, bytes.length, true);
//			if (header.getMaxLength() < MicroFocusFileHeader.MIN_4_BYTE_LENGTH) {
//				Conversion.setLong(len, 0, 2, bytes.length, true);
//			} else {
//				Conversion.setLong(len, 0, 4, bytes.length, true);
//			}
		} catch (Exception e) {
			e.printStackTrace();
			throw new IOException("Error Setting Microfocus Record Length");
		}
		
		int remainder;
		int tmp = len[0];
		tmp |= 64;
		
		len[0] = (byte) tmp;
		
		outStream.write(len);
		outStream.write(bytes);
		
		remainder = (bytes.length + recLengthbytes) % 4;
		
		if (remainder != 0) {
			outStream.write(new byte[4 - remainder]);
		}
	}

	@Override
	public void close() throws IOException {
		header = null;
		outStream.close();
	}

}
