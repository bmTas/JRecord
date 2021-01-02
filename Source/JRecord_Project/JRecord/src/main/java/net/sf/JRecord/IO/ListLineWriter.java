package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import net.sf.JRecord.Details.AbstractLine;


/**
 * Write `Lines` to a list
 * @author Bruce Martin
 *
 */
public class ListLineWriter extends AbstractLineWriter {

	private final ArrayList<AbstractLine> lines = new ArrayList<AbstractLine>();
	@Override
	public void open(OutputStream outputStream) throws IOException {
		lines.clear();
	}

	@Override
	public void write(AbstractLine line) throws IOException {
		lines.add(line);
	}

	@Override
	public void close() throws IOException { }

	public ArrayList<AbstractLine> getLines() {
		return lines;
	}

}
