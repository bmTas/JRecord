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
import java.util.Arrays;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;

/**
 * Reads a standard Text file (like standard readLine of Class BufferedReader) except it will return an
 * Array of Bytes (instead of a String). This allows binary data to be in a line (i.e. using X'FF' as a field
 * Separator). It has a limit of 256kb on the line size.
 *
 * @author  Bruce Martin
 * @version 0.68
 */
public abstract class BaseByteTextReader extends AbstractByteReader {

	protected byte[] crBytes = Constants.CR_BYTES;
	protected byte[] lfBytes = Constants.LF_BYTES;
	protected byte[] lfcrBytes = Constants.CRLF_BYTES;

	protected byte byteLF = Constants.BYTE_LF;
	protected byte byteCR = Constants.BYTE_CR;

	protected static final byte[] EMPTY = {};



	private static FindLines NO_EOL_FINDLINES = new  FindLines() {
			@Override public void findLinesInBuffer(int start) {

			}
		};
	//private static int MAX_LINE_SIZE = 750;
	private static int MAX_LINE_SIZE = BUFFER_SIZE*8;
	private static final byte[] NO_EOL = EMPTY;
	protected byte[] eol = null,
					 altEol = EMPTY;


	protected final byte[] buffer = new byte[MAX_LINE_SIZE];
	protected int bytesInBuffer;
	protected final int[] lineArray = new int[16];
	protected int noLines, lineNo;

	private InputStream in = null;
	private boolean eof, eofPending;
	private long bytesRead = 0;

	protected boolean check4cr = false;
	protected boolean check4lf = false;

	protected FindLines findLines = NO_EOL_FINDLINES;

	public BaseByteTextReader() {
		this("");
	}

	public BaseByteTextReader(String charSet) {
		setLfCr(charSet);
	}

	@Override
	public void open(InputStream inputStream) throws IOException {
		in = inputStream;
		eof = false;
		check4lf = false;
		check4cr = false;
//		check4crlf = false;


		bytesInBuffer = readBuffer(in, buffer);
		eofPending = bytesInBuffer < buffer.length;


		if (eol == null) {
			int eolPos = getEolPosition();

			if (eolPos >= bytesInBuffer) {
				eol = NO_EOL;
			} else if (buffer[eolPos] ==  byteCR) {
				if (eolPos+1 < bytesInBuffer && buffer[eolPos+1] ==  byteLF) {
					eol = lfcrBytes;
					altEol = lfBytes;
//					check4crlf = true;
				} else {
					eol = crBytes;
					check4lf = true;
				}
			} else {
					eol = lfBytes;
					altEol = lfcrBytes;
					check4cr = true;
					
//				}
			}
		}

		setLineSearch();

		findLinesInBuffer(0);
		lineNo = -1;
	}

	protected int getEolPosition() {

		int pos = 0;
		while (pos < bytesInBuffer && buffer[pos] != byteCR && buffer[pos] != byteLF) {
			pos += 1;
		}

		return pos;
	}

	protected void setLineSearch() {
		if (eol.length > 0) {
			findLines = new StdFindLines();
		}
	}


	@Override
	public final byte[] read() throws IOException {
		if (in == null) {
			throw new IOException("File has not been opened");
		}
		if (eof) {
			return null;
		}

		byte[] ret = null;
		int srcPos;
		int lno = getLineNo();
		
		if (eof) {
			if (lineArray.length <= lno || bytesInBuffer <= lineArray[lno]) {
				return null;
			}
			srcPos = lineArray[lno];

			if (srcPos < buffer.length && ((check4cr &&  buffer[srcPos] == byteCR) || (check4lf && buffer[srcPos] == byteLF))) {
				srcPos += 1;
				bytesRead += 1;
			} 
			ret = new byte[bytesInBuffer - srcPos];
		} else { 
			srcPos = lineArray[lno];

			if (srcPos < buffer.length && ((check4cr &&  buffer[srcPos] == byteCR) || (check4lf && buffer[srcPos] == byteLF))) {
				srcPos += 1;
				bytesRead += 1;
			} 
			int eolLength = eol.length;
			if (check4cr
			&& (lno+1 < lineArray.length) && (lineArray[lno+1] - eolLength - 1 < buffer.length)
			&& (buffer[lineArray[lno+1] - eolLength - 1] == byteCR)) {
				eolLength += 1;
			}
			if (lno+1 < lineArray.length) {
				//System.out.print("\t~ " + lno+ " " + lineArray[lno+1] + " - " +  srcPos  + " - " + eolLength + "   " );
				int lineLen = lineArray[lno+1] -  srcPos - eolLength;
				if (lineLen < 0) {
					ret  = EMPTY;
					bytesRead += Math.max(0, lineLen + eolLength);
				} else {
					ret = lineLen == 0 ? EMPTY : new byte[lineLen];
					bytesRead += eolLength;
				}
			} else {
				ret = new byte[buffer.length - srcPos];
				bytesRead += eolLength;
			}
		}
		System.arraycopy(buffer, srcPos, ret, 0, ret.length);
		bytesRead += ret.length;

		return ret;
	}


	@Override
	public final void close() throws IOException {
		if (in != null) {
			in.close();
		}
		in=null;
	}

	/**
	 * @param eol the eol to set
	 */
	public final BaseByteTextReader setEol(byte[] eol) {
		this.eol = eol;
		if (Arrays.equals(eol, lfBytes)) {
			altEol = lfcrBytes;
			check4cr = true;
		} else if (Arrays.equals(eol, lfcrBytes)) {
			altEol = lfBytes;
		} else if (Arrays.equals(eol, crBytes)) {
			check4cr = true;
		}
		return this;
	}

	private int getLineNo() throws IOException {

		lineNo += 1;
		if (lineNo == noLines - 1) {
			if (eofPending && noLines < lineArray.length) {
				eof = true;
			} else {
				findLinesInBuffer(lineArray[lineNo]);
				if (noLines == 1) {
					if (eofPending) {
						eof = true;
					} else {
						int len = bytesInBuffer - lineArray[lineNo];
						System.arraycopy(buffer, lineArray[lineNo], buffer, 0, len);

						bytesInBuffer = readBuffer(in, buffer, len);
						eofPending = bytesInBuffer < buffer.length;

						findLinesInBuffer(0);
					}
				}
			}
		}

		return lineNo;
	}

	private void findLinesInBuffer(int start) {
		lineArray[0] = start;
		noLines = 1;
		lineNo = 0;
		findLines.findLinesInBuffer(start);
	}


	/**
	 * @return the eol
	 */
	public final byte[] getEol() {
		return eol;
	}


	protected final void setLfCr(String charSet) {

		byte[] b = Conversion.getBytes("\r\n", charSet);

		if (b[0] != byteLF || b[1] != byteCR) {
			byteCR = b[0];
			byteLF = b[1];

			crBytes = new byte[] {byteCR};
			lfBytes = new byte[] {byteLF};
			lfcrBytes = b; //new byte[] {byteLF, byteCR};
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.AbstractByteReader#getBytesRead()
	 */
	@Override
	public long getBytesRead() {
		return bytesRead;
	}

	protected static interface FindLines {
		public void findLinesInBuffer(int start);
	}

	protected class StdFindLines implements FindLines {

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.BaseByteTextReader.FindLines#findLinesInBuffer(int)
		 */
		@Override
		public void findLinesInBuffer(int start) {
			byte last = -128;
			int idx = eol.length - 1;

			while (noLines < lineArray.length && start < bytesInBuffer && start >= 0) {
				if ((buffer[start] == eol[idx]) && (eol.length == 1 || last == eol[0])) {
					lineArray[noLines] = start+1;
					noLines += 1;
				}

				last = buffer[start];
				start += 1;
			}
		}
	}


}
