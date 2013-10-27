package net.sf.JRecord.ByteIO;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;


/**
 * Reads a standard Text file (like standard readLine of Class BufferedReader) except it will return an
 * Array of Bytes (instead of a String). This allows binary data to be in a line (i.e. using X'FF' as a field
 * seperator). It has a limit of 256kb on the line size.
 *
 * @author  Bruce Martin
 * @version 0.68
 */
public class ByteTextReader extends BaseByteTextReader {

	public ByteTextReader() {
		super();
	}

	public ByteTextReader(String charSet) {
		super(charSet);
	}

	public static List<byte[]> readStream(String charSet, InputStream in, int maxNoLines) throws IOException {
		ArrayList<byte[]> ret = new ArrayList<byte[]>();
		byte[] b;
		boolean all = maxNoLines < 0;
		ByteTextReader r = new ByteTextReader(charSet);

		r.open(in);

		while ((all || ret.size() < maxNoLines)
			&& (b = r.read()) != null) {
			ret.add(b);
		}

		r.close();

		return ret;
	}
}
