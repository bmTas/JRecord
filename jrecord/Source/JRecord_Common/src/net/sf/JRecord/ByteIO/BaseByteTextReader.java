package net.sf.JRecord.ByteIO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;

/**
 * Reads a standard Text file (like standard readLine of Class BufferedReader) except it will return an
 * Array of Bytes (instead of a String). This allows binary data to be in a line (i.e. using X'FF' as a field
 * seperator). It has a limit of 256kb on the line size.
 *
 * @author  Bruce Martin
 * @version 0.68
 */
public abstract class BaseByteTextReader extends AbstractByteReader {

	protected byte[] crBytes = Constants.CR_BYTES;
	protected byte[] lfBytes = Constants.LF_BYTES;
	protected byte[] lfcrBytes = Constants.LFCR_BYTES;

	protected byte byteLF = Constants.BYTE_LF;
	protected byte byteCR = Constants.BYTE_CR;

	protected static final byte[] EMPTY = {};



	private static FindLines NO_EOL_FINDLINES = new  FindLines() {
			@Override public void findLinesInBuffer(int start) {

			}
		};
	//private static int MAX_LINE_SIZE = 750;
	private static int MAX_LINE_SIZE = BUFFER_SIZE*16;
	private static final byte[] NO_EOL = EMPTY;
	protected byte[] eol = null;


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


		bytesInBuffer = readBuffer(in, buffer);
		eofPending = bytesInBuffer < buffer.length;


		if (eol == null) {
			int eolPos = getEolPosition();

			if (eolPos >= bytesInBuffer) {
				eol = NO_EOL;
			} else if (buffer[eolPos] ==  byteCR) {
				eol = crBytes;
				check4lf = true;
			} else {
				if (eolPos+1 < bytesInBuffer && buffer[eolPos+1] ==  byteCR) {
					eol = lfcrBytes;
				} else {
					eol = lfBytes;
					check4cr = true;
				}
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
		int lno = getLineNo();

//		if (eof && lno >= lineArray.length) {
//			return null;
//		}

		int srcPos = lineArray[lno];

		if (check4cr && srcPos < buffer.length && buffer[srcPos] == byteCR) {
			srcPos += 1;
			bytesRead += 1;
		}
		if (eof) {
			if (bytesInBuffer <= srcPos) {
				return null;
			}
			ret = new byte[bytesInBuffer - srcPos];
		} else {
			int eolLength = eol.length;
			if (check4lf && (buffer[lineArray[lno+1] - eolLength - 1] == byteLF)) {
				eolLength += 1;
			}
			ret = new byte[lineArray[lno+1] -  srcPos - eolLength];
			bytesRead += eolLength;
		}
		System.arraycopy(buffer, srcPos, ret, 0, ret.length);
		bytesRead += ret.length;

		return ret;
	}


	@Override
	public final void close() throws IOException {
		in.close();

		in=null;
	}

	/**
	 * @param eol the eol to set
	 */
	public final BaseByteTextReader setEol(byte[] eol) {
		this.eol = eol;
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


	protected void setLfCr(String charSet) {

		byte[] b = Conversion.getBytes("\r\n", charSet);

		if (b[0] != byteLF || b[1] != byteCR) {
			byteLF = b[0];
			byteCR = b[1];

			crBytes = new byte[] {byteCR};
			lfBytes = new byte[] {byteLF};
			lfcrBytes = new byte[] {byteLF, byteCR};
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
