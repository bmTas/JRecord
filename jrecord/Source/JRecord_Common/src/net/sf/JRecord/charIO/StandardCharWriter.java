package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

public class StandardCharWriter extends BaseCharWriter {

	private final String eol; 
	private final String font;
	
	private Writer w;
	
	
	public StandardCharWriter(String eol, String charset) {
		super();
		this.eol = eol;
		this.font = charset;
	}

	@Override
	public void open(OutputStream outputStream) throws IOException {
		if (font == null || font.length() == 0) {
			w = new OutputStreamWriter(outputStream);
		} else {
			w = new OutputStreamWriter(outputStream, font);
		}
	}

	@Override
	public void write(char[] line) throws IOException {

		w.write(line);
		w.write(eol);
	}
}
