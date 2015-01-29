/**
 *
 */
package net.sf.JRecord.charIO;

import java.io.IOException;

/**
 * An ICharReader implementation using java's standard
 * Reader class to implement it 
 * 
 * @author Bruce Martin
 *
 */
public class StandardCharReader extends BaseCharReader {

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#read()
	 */
	@Override
	public String read() throws IOException {
		return reader.readLine();
	}

}
