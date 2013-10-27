/**
 *
 */
package net.sf.JRecord.charIO;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * @author mum
 *
 */
public class StandardCharReader implements ICharReader {

	private BufferedReader reader;


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
		InputStreamReader stdReader;
		if (font == null || "".equals(font)) {
		    stdReader = new InputStreamReader(inputStream);
		} else {
		    try {
		        stdReader = new InputStreamReader(inputStream, font);
		    } catch (Exception e) {
 		        stdReader = new InputStreamReader(inputStream);
		    }
		}

		reader = new BufferedReader(stdReader);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#read()
	 */
	@Override
	public String read() throws IOException {
		return reader.readLine();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.charIO.ICharReader#close()
	 */
	@Override
	public void close() throws IOException {
		reader.close();
	}

}
