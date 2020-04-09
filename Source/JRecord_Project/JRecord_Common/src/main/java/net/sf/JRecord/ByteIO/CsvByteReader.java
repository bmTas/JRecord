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
      
package net.sf.JRecord.ByteIO;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;

/**
 *
 * Reads a file one line at a time (used for single byte character sets only).
 *`
 * @author Bruce Martin
 *
 *
 */
public class CsvByteReader extends BaseByteTextReader {

	private final byte[] fieldSep;
	private final byte[] quote;
	private final byte[] quoteEsc;
	private final byte[] sepQuote;
	private final byte[] quoteSep;
	private byte[] quoteEol = EMPTY;
	private byte[] quoteEol2 = EMPTY;

	private boolean useStdEolCheck;


	public CsvByteReader(String charSet, byte[] fieldSep, byte[] quote, String quoteEsc, boolean useStdEolCheck) {
		this(	null,
				fieldSep,
				quote,
				Conversion.getBytes(quoteEsc, charSet),
				useStdEolCheck);

		setLfCr(charSet);
	}

	public CsvByteReader(byte[] eol, byte[] fieldSep, byte[] quote, byte[] quoteEsc, boolean useStdEolCheck) {
		super();
		super.setEol(eol);
		this.fieldSep = fieldSep;
		this.quoteEsc = quoteEsc == null ? EMPTY : quoteEsc;
		this.useStdEolCheck = useStdEolCheck;

		if (quote == null || quote.length == 0) {
			this.quote = EMPTY;
			this.sepQuote = EMPTY;
			this.quoteSep = EMPTY;
		} else {
			this.quote = quote;
			this.sepQuote = new byte[fieldSep.length + quote.length];
			this.quoteSep =  new byte[sepQuote.length];

			System.arraycopy(fieldSep, 0, sepQuote, 0, fieldSep.length);
			System.arraycopy(quote,    0, sepQuote,    fieldSep.length, quote.length);

			System.arraycopy(quote,   0,  quoteSep, 0, quote.length);
			System.arraycopy(fieldSep, 0, quoteSep,    quote.length, fieldSep.length);
//			System.out.println("sepQuote: " + sepQuote[0] + "\t" + sepQuote[1]);
//			System.out.println("quoteSep: " + quoteSep[0] + "\t" + quoteSep[1]);
		}

//		if (quoteEsc.length > 0 && quoteEsc.length == 2 * this.quote.length) {
//			for (int i = 0; i < quoteEsc.length; i++) {
//				if (quoteEsc[i] != quote[i % (quote.length + 1)]) {
//					break;
//				}
//			}
//		}
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.BaseByteTextReader#getEolPosition()
	 */
	@Override
	protected int getEolPosition() {
		if (! useStdEolCheck) {
			byte[] qEol1 = new byte[quote.length + 1];
			byte[] qEol2 = new byte[quote.length + 1];
			byte[] qEol3 = new byte[quote.length +  super.lfcrBytes.length];

			SearchDtls searchDtls = new SearchDtls();

			System.arraycopy(quote,           0, qEol1, 0, quote.length);
			System.arraycopy(super.crBytes,   0, qEol1,    quote.length, super.crBytes.length);
			System.arraycopy(quote,           0, qEol2, 0, quote.length);
			System.arraycopy(super.lfBytes,   0, qEol2,    quote.length, super.lfBytes.length);
			System.arraycopy(quote,           0, qEol3, 0, quote.length);
			System.arraycopy(super.lfcrBytes, 0, qEol3,    quote.length, super.lfcrBytes.length);

			FindLines eolSearch;
			if (quoteEsc.length == 0) {
				eolSearch = new NoQuoteEsc(qEol1, qEol2, qEol3, searchDtls);
			} else {
				eolSearch = new QuoteEsc(qEol1, qEol2, qEol3, searchDtls);
			}
			eolSearch.findLinesInBuffer(0);

			if (searchDtls.noLines > 0) {
				return lineArray[0] - 1;
			}
		}
		return super.getEolPosition();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.BaseByteTextReader#open(java.io.InputStream)
	 */
	@Override
	protected void  setLineSearch() {

		this.quoteEol = eol;
		this.quoteEol2 = EMPTY;

		if (eol.length == 0) {
			this.quoteEol = EMPTY;
		} else if (quote.length == 0) {
			findLines = new StdFindLines();
		} else {
			this.quoteEol = new byte[quote.length + eol.length];
			System.arraycopy(quote, 0, quoteEol, 0, quote.length);
			System.arraycopy(eol,   0, quoteEol,    quote.length, eol.length);
			//System.out.println("quoteEol: " + quoteEol[0] + "\t" + quoteEol[1]);

			if (super.check4lf) {
				this.quoteEol2 = new byte[quote.length + eol.length + 1];
				System.arraycopy(quote, 0, quoteEol2, 0, quote.length);
				quoteEol2[quote.length] = super.byteLF;
				System.arraycopy(eol,   0, quoteEol2,    quote.length+1, eol.length);
			}

//			String eols = "none ";
//			if (eol == null) {
//
//			} else if (eol.length == 1) {
//				eols = "" + (0 + eol[0]);
//			} else if (eol.length >= 2) {
//				eols = "{" + (0 +eol[0]) + ", " +  (0 +eol[1]) + "}";
//			}
			if (quoteEsc.length == 0) {
//				System.out.println("NoQuoteEsc, quote=`" + quote[0] + "` " + eols
//						+ " " + check4lf + " " + check4cr);
				findLines = new NoQuoteEsc();
			} else {
//				System.out.println("QuoteEsc, quote=`" + quote[0] + "` " + eols
//						+ " " + check4lf + " " + check4cr + " " + byteLF + " " + byteCR);
				findLines = new QuoteEsc();
/*				boolean doubleQuoteEsc = false;
				if (quoteEsc.length == 2 * this.quote.length) {
					doubleQuoteEsc = true;
					for (int i = 0; i < quoteEsc.length; i++) {
						if (quoteEsc[i] != quote[i % (quote.length + 1)]) {
							doubleQuoteEsc = false;
							break;
						}
					}
				}

				if (doubleQuoteEsc) {

				} else {

				}*/
			}
		}
	}

//	private static byte[] getEolBytes(String charSet) {
//
//		byte[] b = Conversion.getBytes("\n", charSet);
//		if (b == null || b.length == 0 || b[0] == Constants.BYTE_CR || b[0] == Constants.BYTE_LF) {
//			return null;
//		}
//
//		return b;
//	}

/**	protected void findLinesInBuffer(int start) {

		lineArray[0] = start;
		noLines = 1;
		lineNo = 0;
		findLines.findLinesInBuffer(start);

/*   **********************************************
		byte last = -128;
		int idx = eol.length - 1;
		lineArray[0] = start;
		noLines = 1;
		lineNo = 0;
		int lineStart = 0;
		boolean inQuote = false;
		boolean inLastQuote = false;

		if (idx >= 0) {

			if (quoteEsc.length == 0) {
				while (noLines < lineArray.length && start < bytesInBuffer && start >= 0) {
					if (checkFor(start, sepQuote)) {
						inQuote = true;
					}
				}
			}
			while (noLines < lineArray.length && start < bytesInBuffer && start >= 0) {
				if (checkFor(start, quoteEsc)) {
					inLastQuote = inQuote;
					inQuote = inLastQuote;
				} else if ((lineStart==start && checkFor(start, quote)
						|| (quoteEsc.length == 0 && checkFor(start, sepQuote)))) {
					inQuote = true;
				} else if (quoteEsc.length > 0 && checkFor(start, quote)) {
					inLastQuote = inQuote;
					inQuote = ! inQuote;
				} else if (  checkFor(start, quoteEol)
						|| ((! inQuote) && checkFor(start, eol))) {
					lineStart = start+1;
					lineArray[noLines] = lineStart;
					noLines += 1;
					inLastQuote = false;
					inQuote = false;
				}

				last = buffer[start];
				start += 1;
			}
		}


	}*/

	protected final boolean checkFor(int pos, byte[] search) {
		return CommonBits.checkFor(super.buffer, pos, search);
		//System.out.println("!! " + pos + " " + (search.length - 1) );
//		if (search == null || pos < search.length - 1 || search.length == 0) {
//			return false;
//		}
//
//		int bufferStart = pos - search.length + 1;
//		for (int i = 0; i < search.length; i++) {
//			if (search[i] != super.buffer[bufferStart + i]) {
//				return false;
//			}
//		}
//
//		return true;
	}

	private class NoQuoteEsc implements FindLines {
		private final byte[] quoteEol1;
		private final byte[] quoteEol2 ;
		private final byte[] quoteEol3;

		private final ILineNoDtls dtls;


		public NoQuoteEsc() {
			this(	CsvByteReader.this.quoteEol, CsvByteReader.this.quoteEol2, EMPTY,
					new StdLineNoDtls());
		}

		public NoQuoteEsc(byte[] quoteEol, byte[] quoteEol2, byte[] quoteEol3, ILineNoDtls dtls) {
			super();
			this.quoteEol1 = quoteEol;
			this.quoteEol2 = quoteEol2;
			this.quoteEol3 = quoteEol3;
			this.dtls = dtls;
		}

		public void findLinesInBuffer(int start) {
			int lineStart = start;
			int fieldStart = start;
			boolean inQuote = false;


			while (dtls.getNoLines() < lineArray.length && start < bytesInBuffer && start >= 0) {
				if (checkFor(start, sepQuote)
				|| (lineStart == start - quote.length + 1 && checkFor(start, quote))
				) {
					inQuote = true;
				} else if ( (  inQuote
							&& (fieldStart != start - quoteSep.length + 1)
							&& checkFor(start, quoteSep))
						|| ((! inQuote) && checkFor(start, fieldSep))) {
					fieldStart = start + 1;
					inQuote = false;
				} else if ((	checkFor(start, quoteEol1) && (fieldStart != start - quoteEol1.length + 1))
						|| (	checkFor(start, quoteEol2) && (fieldStart != start - quoteEol2.length + 1))
						|| (	checkFor(start, quoteEol3) && (fieldStart != start - quoteEol3.length + 1))
						|| ((! inQuote) && dtls.isEol(start))) {
					lineStart = start+1;
					fieldStart = lineStart;
					lineArray[dtls.getNoLines()] = lineStart;
					dtls.incNoLines();
					inQuote = false;
				} else if (check4cr && buffer[start] == byteCR && lineStart == start) {
					lineStart += 1;
					fieldStart = lineStart;
				}

				start += 1;
			}
		}
	}

	private class QuoteEsc implements FindLines {
		private final byte[] quoteEol1;
		private final byte[] quoteEol2 ;
		private final byte[] quoteEol3;

		private final ILineNoDtls dtls;


		public QuoteEsc() {
			this(	CsvByteReader.this.quoteEol, CsvByteReader.this.quoteEol2, EMPTY,
					new StdLineNoDtls());
		}

		public QuoteEsc(byte[] quoteEol, byte[] quoteEol2, byte[] quoteEol3, ILineNoDtls dtls) {
			super();
			this.quoteEol1 = quoteEol;
			this.quoteEol2 = quoteEol2;
			this.quoteEol3 = quoteEol3;
			this.dtls = dtls;
		}

		public void findLinesInBuffer(int start) {
			int lineStart = start;
			int fieldStart = start;
			boolean inQuote = false;
			//boolean[] followingQuoteEsc = {false, false, false};
			int quoteEscPos = -121;

			while (dtls.getNoLines() < lineArray.length && start < bytesInBuffer && start >= 0) {
				if (checkFor(start, sepQuote)
				|| ((lineStart == start - quote.length + 1) && checkFor(start, quote)) ) {
					inQuote = true;
				} else if ( dtls.isQuoteEscEol(quoteEscPos, start)
						|| (	quoteEscPos >= start - fieldSep.length
							&&	checkFor(start, fieldSep))
						|| (	quoteEscPos > start - quoteEsc.length
							&&	checkFor(start, quote))) {

				} else if (inQuote
						&& fieldStart <= start - quoteEsc.length - quote.length + 1
						&& checkFor(start, quoteEsc)) {
//					System.out.print("\t>>" + start
//							+ " " + quoteEscPos + " >= " + start + " - " + quote.length
//							+ "<<");
					quoteEscPos = start;
				} else if ( (inQuote && checkFor(start, quoteSep) && (fieldStart != start - quoteSep.length + 1))
						|| ((! inQuote) && checkFor(start, fieldSep))) {
					fieldStart = start + 1;
					inQuote = false;
				} else if ((	checkFor(start, quoteEol1) && (fieldStart != start - quoteEol1.length + 1))
						|| (	checkFor(start, quoteEol2) && (fieldStart != start - quoteEol2.length + 1))
						|| (	checkFor(start, quoteEol3) && (fieldStart != start - quoteEol3.length + 1))
						|| ((! inQuote) && dtls.isEol(start))) {
					lineStart = start+1;
					fieldStart = lineStart;
					lineArray[dtls.getNoLines()] = lineStart;
					dtls.incNoLines();
					inQuote = false;
				} else if (check4cr && buffer[start] == byteCR && lineStart == start) {
					lineStart += 1;
					fieldStart += 1;
				}

				start += 1;
			}
		}
	}

	private static interface ILineNoDtls {

		public int getNoLines();

		public void incNoLines();

		public boolean isQuoteEscEol(int quoteEscPos, int start);

		boolean isEol(int start);
	}

	private class StdLineNoDtls implements ILineNoDtls {

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#getNoLines()
		 */
		@Override
		public int getNoLines() {
			return noLines;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#incNoLines()
		 */
		@Override
		public void incNoLines() {
			noLines += 1;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#isQuoteEscEol()
		 */
		@Override
		public boolean isQuoteEscEol(int quoteEscPos, int start) {
//			if (checkFor(start, eol)) {
//				System.out.println("--> " + quoteEscPos + " " + start + " " + eol.length
//						+ " " + eol[0] + " " + checkFor(start, eol) + " " + buffer[start]);
//			}
			return quoteEscPos == start - eol.length
					&&	checkFor(start, eol);
		}


		@Override
		public boolean isEol(int start) {
			return checkFor(start, eol);
		}
	}


	private class SearchDtls implements ILineNoDtls {
		int noLines = 0;

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#getNoLines()
		 */
		@Override
		public int getNoLines() {
			return noLines;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#incNoLines()
		 */
		@Override
		public void incNoLines() {
			noLines += 1;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.CsvByteReader.ILineNoDtls#isQuoteEscEol()
		 */
		@Override
		public boolean isQuoteEscEol(int quoteEscPos, int start) {
			return (quoteEscPos == start - lfBytes.length
					&&	(checkFor(start, lfBytes) || checkFor(start, crBytes)))
				|| (quoteEscPos == start - lfcrBytes.length && checkFor(start, lfcrBytes))
						;
		}


		@Override
		public boolean isEol(int start) {
			return checkFor(start, lfBytes)
				|| checkFor(start, crBytes)
			;
		}

	}

}
