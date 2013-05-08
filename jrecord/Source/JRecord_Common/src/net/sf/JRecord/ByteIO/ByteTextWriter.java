package net.sf.JRecord.ByteIO;

import net.sf.JRecord.Common.Constants;

/**
 * This class will write an Array of Bytes to a file then write the appropriate End-Of-Line characters to the file.
 * This allows hex field seperators to be used.
 *
 * @author Bruce Martin
 * @version 0.68
 */
public class ByteTextWriter extends FixedLengthByteWriter {

	/**
	 * Standard Text file Byte writer using System End-of-Line
	 */
	public ByteTextWriter() {
		super(false, false, Constants.SYSTEM_EOL_BYTES);
	}

	/**
	 * Standard Text file Byte writer using user supplied End-of-Line
	 * @param eol end of line
	 */
	public ByteTextWriter(String eol) {
		super(false, false, eol.getBytes());
	}

	/**
	 * Standard Text file Byte writer using user supplied End-of-Line
	 * @param eol end of line
	 */
	public ByteTextWriter(byte[] eol) {
		super(false, false, eol);
	}
}
