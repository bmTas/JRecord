/**
 *
 */
package net.sf.JRecord.charIO;

import java.io.IOException;

/**
 * An ICharReader implementation read fixed length records
 * from a file as a String 
 * 
 * @author Bruce Martin
 *
 */
public class FixedLengthCharReader extends BaseCharReader {
	private final int length;
	private boolean eofPending = false;
	
	public FixedLengthCharReader(int length) {
		super();
		this.length = length;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#read()
	 */
	@Override
	public String read() throws IOException {
		if (eofPending) return null;

		char[] c = new char[length];

		int l = reader.read(c, 0, length);
		if (l < 0) {
			return null;
		}

		int read = l;
		while (read < length && (l = reader.read(c, read, length - read)) >= 0) {
			read += l;
		}
		eofPending = l < 0;
		if (read < length) {
			char[] tmp = new char[read];
			System.arraycopy(c, 0, tmp, 0, read);
			c = tmp;
		}
			
		return new String(c);
	}

}
