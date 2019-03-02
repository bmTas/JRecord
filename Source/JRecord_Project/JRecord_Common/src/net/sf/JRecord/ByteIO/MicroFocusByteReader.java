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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;

import net.sf.JRecord.Common.Conversion;

public class MicroFocusByteReader extends AbstractByteReader {

	
	private MicroFocusFileHeader headerRecord;
	private BufferedInputStream instream;
	private boolean eof = true;

	private byte[] relativeAttr = new byte[2];
	
	private int lineNumber = 1;
	byte[] len1;

	@Override
	public void open(InputStream inputStream) throws IOException {
		instream = new BufferedInputStream(inputStream, 8192);
		
		byte[] headerRec = new byte[128];
		int ii = readBuffer(instream, headerRec);
//		if ( ! (eof = (instream.read(headerRec) <= 0))) {
		if ( ! (eof = (ii <= 0))) {
			headerRecord = new MicroFocusFileHeader(headerRec);
			if (headerRecord.getCompression() > 0 ) {
				throw new IOException("Compression is not supported");
//			} else if (headerRecord.isValidFile()) {
//				throw new IOException("Microfocus File is corrupt (flag in header record)");
			}
			
			if (headerRecord.getMaxLength() < MicroFocusFileHeader.MIN_4_BYTE_LENGTH) {
				len1 = new byte[2];
			} else {
				len1 = new byte[4];
			}
		}
	}

	@Override
	public byte[] read() throws IOException {
		
		byte[] rec;
		int tmp, attr, len, remainder;
		boolean readnext = true;
		
		do {
			if (eof || readBuffer(instream, len1) <= 0) {
				eof = true;
				return null;
			}
			tmp = len1[0];
			attr = (tmp & 240) >> 4;
			
			//System.out.print("Initial: " + tmp + " Attr: " + attr);
			len1[0] = (byte) (tmp & 15);
			
			
			len = new BigInteger(len1).intValue();
			
			readnext = ! (attr == 4 || attr == 5 || attr == 7 || attr == 8);
			if (len > 1000000) {
				throw new IOException("Record Length to Big: " + len + " record number: " + lineNumber);
			}
			rec = new byte[len];
			
			int bytesRead;
			if ((bytesRead = readBuffer(instream, rec)) < len) {
				throw new IOException("Length of Record (" + bytesRead + ") < Expected (" + len + ") record number: " + lineNumber);
			}
			byte[] buf;
//			System.out.println();
//			System.out.println("Atrr " + attr + " " + readnext);
//			System.out.println("Hex: " + Conversion.getDecimal(rec, 0, rec.length));
//			System.out.println("Str: " + new String(rec));
			switch (headerRecord.getFileFormat()) {
			case  MicroFocusFileHeader.FORMAT_INDEXED:
			case  MicroFocusFileHeader.FORMAT_SEQUENTIAL:
				remainder = (len + len1.length) % 4;
				if (remainder != 0) {
					buf = new byte[4 - remainder];
					readBuffer(instream, buf);
//					System.out.println("Remainder: " + Conversion.getDecimal(buf, 0, buf.length) + " " + new String(buf));
				}
				break;
//			case  MicroFocusFileHeader.FORMAT_INDEXED:
//				break;
			case  MicroFocusFileHeader.FORMAT_RELATIVE:
				if (len < headerRecord.getMaxLength()) {
					readBuffer(instream, new byte[headerRecord.getMaxLength() - len]);
				}
				readBuffer(instream, relativeAttr);
				readnext |= (relativeAttr[0] == 13 && relativeAttr[1] == 10);
				break;				
			}
		
		} while (readnext);
		
		lineNumber += 1;
		return rec;
	}

	@Override
	public void close() throws IOException {
		instream.close();
	}


	public MicroFocusFileHeader getHeaderRecord() {
		return headerRecord;
	}

	@Override
	public boolean canWrite() {

		if (headerRecord == null) {
			return true;
		}
		return headerRecord.getFileFormat() == MicroFocusFileHeader.FORMAT_SEQUENTIAL;
	}

	
}
