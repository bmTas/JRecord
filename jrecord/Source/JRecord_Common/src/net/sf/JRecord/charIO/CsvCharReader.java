/**
 *
 */
package net.sf.JRecord.charIO;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;



/**
 * Read a logical Csv Record from a file.
 * A Csv Record may have embedded <EOL> chars
 * in it (the field must have quote characters).
 * 
 * @author Bruce Martin
 *
 */
public class CsvCharReader implements ICharReader {

	private static final char[] CR_CHARS = {'\n'};
	private static final char[] LFCR_CHARS = {'\r', '\n'};
	private static final char[] LF_CHARS = {'\r'};

	private boolean useStdEolCheck;

	private static final char[] EMPTY = {};
	private static final char[] NO_EOL = EMPTY;

	private final static char charLF = '\r';
	private final static char charCR = '\n';

	private static FindLines NO_EOL_FINDLINES = new  FindLines() {
		@Override public void findLinesInBuffer(int start) {

		}
	};

	public static final int BUFFER_SIZE = 16384;
	protected final char[] buffer = new char[BUFFER_SIZE * 16];

	private final char[] fieldSep;
	private final char[] quote;
	private final char[] quoteEsc;
	private final char[] sepQuote;
	private final char[] quoteSep;

	private char[] quoteEol = EMPTY;
	private char[] quoteEol2 = EMPTY;
	private char[] eol = null;

	private InputStream inStream = null;
	private Reader in = null;
	private boolean eof, eofPending;

	private int charsInBuffer;
	private final int[] lineArray = new int[16];
	private int noLines, lineNo;
//	private long bytesRead = 0;

	private boolean check4cr = false;
	private boolean check4lf = false;


	protected FindLines findLines = NO_EOL_FINDLINES;

	public CsvCharReader(String fieldSep, String quote, String quoteEsc, boolean useStdEolCheck) {
		this(	null,
				fieldSep.toCharArray(),
				quote == null ? null : quote.toCharArray(),
				quoteEsc == null ? null : quoteEsc.toCharArray(),
				useStdEolCheck);
	}


	public CsvCharReader(char[] eol, char[] fieldSep, char[] quote, char[] quoteEsc, boolean useStdEolCheck) {
		super();
		this.eol = eol;
		this.fieldSep = fieldSep;
		this.quoteEsc = quoteEsc == null ? EMPTY : quoteEsc;
		this.useStdEolCheck = useStdEolCheck;

		if (quote == null || quote.length == 0) {
			this.quote = EMPTY;
			this.sepQuote = EMPTY;
			this.quoteSep = EMPTY;
		} else {
			this.quote = quote;
			this.sepQuote = new char[fieldSep.length + quote.length];
			this.quoteSep =  new char[sepQuote.length];

			System.arraycopy(fieldSep, 0, sepQuote, 0, fieldSep.length);
			System.arraycopy(quote,    0, sepQuote,    fieldSep.length, quote.length);

			System.arraycopy(quote,   0,  quoteSep, 0, quote.length);
			System.arraycopy(fieldSep, 0, quoteSep,    quote.length, fieldSep.length);
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#open(java.lang.String)
	 */
	@Override
	public void open(String fileName, String font) throws IOException {
        open(new FileInputStream(fileName), font);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#open(java.io.InputStream)
	 */
	@Override
	public void open(InputStream inputStream, String font) throws IOException {
		this.inStream = inputStream;
		if (font == null || font.length() == 0) {
			this.in = new InputStreamReader(inputStream);
		} else {
			this.in = new InputStreamReader(inputStream, font);
		}
		eof = false;


		charsInBuffer = readBuffer(in, buffer, 0);
		eofPending = charsInBuffer < buffer.length;


		if (eol == null) {
			int size = 0;

			if (! useStdEolCheck) {
				char[] qEol1 = new char[quote.length + 1];
				char[] qEol2 = new char[quote.length + 1];
				char[] qEol3 = new char[quote.length +  LFCR_CHARS.length];

				SearchDtls searchDtls = new SearchDtls();

				System.arraycopy(quote,           0, qEol1, 0, quote.length);
				System.arraycopy(CR_CHARS,   0, qEol1,    quote.length, CR_CHARS.length);
				System.arraycopy(quote,           0, qEol2, 0, quote.length);
				System.arraycopy(LF_CHARS,   0, qEol2,    quote.length, LF_CHARS.length);
				System.arraycopy(quote,           0, qEol3, 0, quote.length);
				System.arraycopy(LFCR_CHARS, 0, qEol3,    quote.length, LFCR_CHARS.length);

				FindLines eolSearch;
				if (quoteEsc.length == 0) {
					eolSearch = new NoQuoteEsc(qEol1, qEol2, qEol3, searchDtls);
				} else {
					eolSearch = new QuoteEsc(qEol1, qEol2, qEol3, searchDtls);
				}
				eolSearch.findLinesInBuffer(0);

				useStdEolCheck = true;
				if (searchDtls.noLines > 0) {
					size =  lineArray[0] - 1;
					useStdEolCheck = false;
				}

			}
			if (useStdEolCheck) {
				while (size < charsInBuffer && buffer[size] != charCR && buffer[size] != charLF) {
					size += 1;
				}
			}

			if (size >= charsInBuffer) {
				eol = NO_EOL;
			} else if (buffer[size] ==  charCR) {
				eol = CR_CHARS;
				check4lf = true;
//				System.out.println("CR = CR "  + font);
			} else {
				if (size+1 < charsInBuffer && buffer[size+1] ==  charCR) {
					eol = LFCR_CHARS;
//					System.out.println("CR = LFCR " + font);
				} else {
					eol = LF_CHARS;
					check4cr = true;
//					System.out.println("CR = LF " + font);
				}
			}
		}

		setLineSearch();

		findLinesInBuffer(0);
		lineNo = -1;

	}


	@Override
	public final String read() throws IOException {
		if (in == null) {
			throw new IOException("File has not been opened");
		}
		if (eof) {
			return null;
		}

		char[] ret = null;
		int lno = getLineNo();
		int srcPos = lineArray[lno];

		if (check4cr && srcPos < buffer.length && buffer[srcPos] == charCR) {
			srcPos += 1;
		}

		if (eof) {
			if (charsInBuffer <= srcPos) {
				return null;
			}
			ret = new char[charsInBuffer - srcPos];
		} else {
			int eolLength = eol.length;
			if (check4lf && (buffer[lineArray[lno+1] - eolLength - 1] == charLF)) {
				eolLength += 1;
			}
//			System.out.println("--> " + lno + " ! "+ lineArray[lno+1] + " - " + srcPos + " - " + eolLength
//					+ " ! " + (lineArray[lno+1] -  srcPos - eolLength));
			ret = new char[lineArray[lno+1] -  srcPos - eolLength];
//			bytesRead += eol.length;
		}
		System.arraycopy(buffer, srcPos, ret, 0, ret.length);
//		bytesRead += ret.length;

		return new String(ret);
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#close()
	 */
	@Override
	public void close() throws IOException {
		if (in != null) {
			inStream.close();
			in.close();

			in = null;
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.ByteIO.BaseByteTextReader#open(java.io.InputStream)
	 */
	protected void  setLineSearch() {

		this.quoteEol = eol;

		if (eol.length == 0) {
			this.quoteEol = EMPTY;
		} else if (quote.length == 0) {
			findLines = new StdFindLines();
		} else {
			this.quoteEol = new char[quote.length + eol.length];
			System.arraycopy(quote, 0, quoteEol, 0, quote.length);
			System.arraycopy(eol,   0, quoteEol,    quote.length, eol.length);

			if (check4lf) {
				this.quoteEol2 = new char[quote.length + eol.length + 1];
				System.arraycopy(quote, 0, quoteEol2, 0, quote.length);
				quoteEol2[quote.length] = '\r';
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
//				System.out.println("Char QuoteEsc, quote=`" + quote[0] + "` " + eols
//						+ " " + check4lf + " " + check4cr );
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
						int len = charsInBuffer - lineArray[lineNo];
						System.arraycopy(buffer, lineArray[lineNo], buffer, 0, len);

						charsInBuffer = readBuffer(in, buffer, len);
						eofPending = charsInBuffer < buffer.length;

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

	protected final boolean checkFor(int pos, char[] search) {
		//System.out.println("!! " + pos + " " + (search.length - 1) );
		if (pos < search.length - 1 || search.length == 0) {
			return false;
		}

		int bufferStart = pos - search.length + 1;
		for (int i = 0; i < search.length; i++) {
			if (search[i] != buffer[bufferStart + i]) {
				return false;
			}
		}

		return true;
	}


	private static interface FindLines {
		public void findLinesInBuffer(int start);
	}


	private class StdFindLines implements FindLines {

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.BaseByteTextReader.FindLines#findLinesInBuffer(int)
		 */
		@Override
		public void findLinesInBuffer(int start) {
			char last = 0;
			int idx = eol.length - 1;

			while (noLines < lineArray.length && start < charsInBuffer && start >= 0) {
				if ((buffer[start] == eol[idx]) && (eol.length == 1 || last == eol[0])) {
					lineArray[noLines] = start+1;
					noLines += 1;
				}

				last = buffer[start];
				start += 1;
			}
		}

	}
	private class NoQuoteEsc implements FindLines {
		private final char[] quoteEol1;
		private final char[] quoteEol2 ;
		private final char[] quoteEol3;

		private final ILineNoDtls dtls;


		public NoQuoteEsc() {
			this(	CsvCharReader.this.quoteEol, CsvCharReader.this.quoteEol2, EMPTY,
					new StdLineNoDtls());
		}

		public NoQuoteEsc(char[] quoteEol, char[] quoteEol2, char[] quoteEol3, ILineNoDtls dtls) {
			this.quoteEol1 = quoteEol;
			this.quoteEol2 = quoteEol2;
			this.quoteEol3 = quoteEol3;
			this.dtls = dtls;
		}


		public void findLinesInBuffer(int start) {
			int lineStart = start;
			int fieldStart = start;
			boolean inQuote = false;


			while (dtls.getNoLines() < lineArray.length && start < charsInBuffer && start >= 0) {
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
				} else if (check4cr && buffer[start] == charCR && lineStart == start) {
					lineStart += 1;
					fieldStart = lineStart;
				}

				start += 1;
			}
		}
	}

	private class QuoteEsc implements FindLines {
		private final char[] quoteEol1;
		private final char[] quoteEol2 ;
		private final char[] quoteEol3;

		private final ILineNoDtls dtls;


		public QuoteEsc() {
			this(	CsvCharReader.this.quoteEol, CsvCharReader.this.quoteEol2, EMPTY,
					new StdLineNoDtls());
		}

		public QuoteEsc(char[] quoteEol, char[] quoteEol2, char[] quoteEol3, ILineNoDtls dtls) {
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

			while (dtls.getNoLines() < lineArray.length && start < charsInBuffer && start >= 0) {
//			while (noLines < lineArray.length && start < charsInBuffer && start >= 0) {
//				followingQuoteEsc[2] = followingQuoteEsc[1];
//				followingQuoteEsc[1] = followingQuoteEsc[0];
//				followingQuoteEsc[0] = false;
//				bb[0] = buffer[start];
//				System.out.print(new String(bb));
//				if ( checkFor(start, eol)) {
//					System.out.print("\t" + inQuote
//							+ " " + followingQuoteEsc[1] + "/" + followingQuoteEsc[2]
//							+ " " + ( (inQuote) && checkFor(start, quoteEsc))
//							+ " " + ( (! inQuote) && checkFor(start, eol))
//							+ " " + checkFor(start, quoteEol)
//							+ " " + buffer[start-1] + " " + buffer[start]
//							+ " " + quoteEol[0] + " " + quoteEol[1]+ "\t");
//					checkFor(start, quoteEol);
//				}
//				System.out.print("\t~" + start + " " + buffer[start] );
//				if (checkFor(start, quote)) {
//					System.out.println( " ~~ " + lineStart + " " + start + " - " + quote.length);
//				}
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
//					System.out.print(" d)");
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
//					System.out.println();
//					System.out.println("=========================== " + noLines);
				} else if (check4cr && buffer[start] == charCR && lineStart == start) {

					//System.out.print(" *)");
					lineStart += 1;
					fieldStart += 1;
				}

				start += 1;
			}
		}
	}


	protected final int readBuffer(Reader in, final char[] buf, int total)
	throws IOException {

		int num = in.read(buf, total, buf.length - total);
//		if (num < 0) {
//			return total;
//		}

		while (num >= 0 && total + num < buf.length) {
			total += num;
			num = in.read(buf, total, buf.length - total);
		}

		if (num > 0) {
			total += num;
		}

		return total;
	}

	public String getEol() {
		return new String(eol);
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
			return (quoteEscPos == start - LF_CHARS.length
					&&	(checkFor(start, LF_CHARS) || checkFor(start, CR_CHARS)))
				|| (quoteEscPos == start - LFCR_CHARS.length && checkFor(start, LFCR_CHARS))
						;
		}


		@Override
		public boolean isEol(int start) {
			return checkFor(start, LF_CHARS)
				|| checkFor(start, CR_CHARS)
			;
		}

	}

}
