package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Arrays;

public class FixedLengthCharWriter extends BaseCharWriter {

	private final int length; 
	private final String font;
	
	public FixedLengthCharWriter(int length, String charset) {
		super();
		this.length = length;
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
		if (line.length > length) {
			w.write(line, 0, length);
		} else {
			char[] c = new char[length - line.length];
			Arrays.fill(c, ' ');
			w.write(line);
			w.write(c);
		}
	}
}
