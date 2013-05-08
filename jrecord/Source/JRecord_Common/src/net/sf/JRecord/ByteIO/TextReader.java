package net.sf.JRecord.ByteIO;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import net.sf.JRecord.Common.Conversion;

public class TextReader extends AbstractByteReader {

	private InputStream inStream;
	private InputStreamReader stdReader;
	private String font;
	private BufferedReader reader = null;
	
	
	/**
	 * @param font
	 */
	public TextReader(String font) {
		this.font = font;
	}


	@Override
	public void open(InputStream inputStream) throws IOException {
       inStream = inputStream;

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

	@Override
	public byte[] read() throws IOException {
		return Conversion.getBytes(reader.readLine(), font);
	}

	@Override
	public void close() throws IOException {
		stdReader.close();
		inStream.close();
	}

}
