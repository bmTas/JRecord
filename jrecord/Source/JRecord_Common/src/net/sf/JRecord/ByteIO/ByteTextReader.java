package net.sf.JRecord.ByteIO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.Constants;

/**
 * Reads a standard Text file (like standard readLine of Class BufferedReader) except it will return an
 * Array of Bytes (instead of a String). This allows binary data to be in a line (i.e. using X'FF' as a field 
 * seperator).
 * 
 * @author  Bruce Martin
 * @version 0.68
 */
public class ByteTextReader extends AbstractByteReader {

	//private static int MAX_LINE_SIZE = 750;
	private static int MAX_LINE_SIZE = BUFFER_SIZE*4;
	private static final byte[] NO_EOL = {};
	private byte[] eol = null;
	

	private byte[] buffer = new byte[MAX_LINE_SIZE];
	private int bytesInBuffer;
	private int[] lineArray = new int[16];
	private int noLines, lineNo;
	
	private InputStream in = null;
	private boolean eof, eofPending;
	private long bytesRead = 0;
	
	@Override
	public void open(InputStream inputStream) throws IOException {
		in = inputStream;
		eof = false;
		

		bytesInBuffer = readBuffer(in, buffer);
		eofPending = bytesInBuffer < buffer.length;
		
		int size = 0;
		
		while (size < bytesInBuffer && buffer[size] != Constants.BYTE_CR && buffer[size] != Constants.BYTE_LF) {
			size += 1;
		}
		
		if (size >= bytesInBuffer) {
			eol = NO_EOL;
		} else if (buffer[size] ==  Constants.BYTE_CR) {
			eol = Constants.CR_BYTES;
		} else {
			if (size+1 < bytesInBuffer && buffer[size+1] ==  Constants.BYTE_CR) {
				eol = Constants.LFCR_BYTES;
			} else {
				eol = Constants.LF_BYTES;
			}
		}

		findLinesInBuffer(0);
		lineNo = -1;
	}

	@Override
	public byte[] read() throws IOException {
		if (in == null) {
			throw new IOException("File has not been opened");
		}
		if (eof) {
			return null;
		}

		byte[] ret = null;
		int lno = getLineNo();
		
		if (eof) {
			if (bytesInBuffer <= lineArray[lno]) {
				return null;
			}
			ret = new byte[bytesInBuffer - lineArray[lno]];
		} else {
			ret = new byte[lineArray[lno+1] -  lineArray[lno] - eol.length];
			bytesRead += eol.length;
		}
		System.arraycopy(buffer, lineArray[lno], ret, 0, ret.length);
		bytesRead += ret.length;
		
		return ret;
	}

	@Override
	public void close() throws IOException {
		in.close();
		
		in=null;
	}

	/**
	 * @param eol the eol to set
	 */
	public final ByteTextReader setEol(byte[] eol) {
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
	
						bytesInBuffer = readBuffer(buffer, len);
						eofPending = bytesInBuffer < buffer.length;
					
						findLinesInBuffer(0);
					}
				}
			}
		}
		
		return lineNo;
	}

	private void findLinesInBuffer(int start) {
		byte last = -128;
		int idx = eol.length - 1;
		lineArray[0] = start;
		noLines = 1;
		lineNo = 0;
		
		while (noLines < lineArray.length && start < bytesInBuffer) {
			if ((buffer[start] == eol[idx]) && (eol.length == 1 || last == eol[0])) {
				lineArray[noLines] = start+1;
				noLines += 1;
			}
			
			last = buffer[start];
			start += 1;
		}
	}
	
	
	protected final int readBuffer(final byte[] buf, int total)
	throws IOException {
		int num;

		num = in.read(buf, total, buf.length - total);

		while (num >= 0 && total + num < buf.length) {
			total += num;
			num = in.read(buf, total, buf.length - total);
		}
		
		if (num > 0) {
			total += num;
		}

		return total;
	}
	
	@Override
	public long getBytesRead() {
		return bytesRead;
	}
	
	/**
	 * @return the eol
	 */
	public byte[] getEol() {
		return eol;
	}

}
