/**
 * 
 */
package net.sf.cobolToJson.impl.readJson;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

/**
 * @author Bruce01
 *
 */
public class ToJRecordFile extends AFieldsToJRecLine {

	private AbstractLine line;
	private final ISchemaIOBuilder ioBldr;
	private final AbstractLineWriter writer;
	private final boolean singleObject;
	
	public ToJRecordFile(ISchemaIOBuilder ioBldr, AbstractLineWriter writer) {
		this(ioBldr, writer, false);
	}
	public ToJRecordFile(ISchemaIOBuilder ioBldr, AbstractLineWriter writer, boolean singleObject) {
		super();
		this.ioBldr = ioBldr;
		this.writer = writer;
		this.singleObject = singleObject;
	}

	/* (non-Javadoc)
	 * @see net.sf.cobolToJson.impl.readJson.IProcessFields#newObject(int, java.lang.String)
	 */
	@Override
	public void endObject(int level, String fieldName) {
		if (level == 1 && ! singleObject) {
			try {
				writeLine();
			
				line = null;
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	private void writeLine() throws IOException {
		if (line != null) {
			writer.write(line);
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.cobolToJson.impl.readJson.AProcessFields#getLine()
	 */
	@Override
	protected AbstractLine getLine() {
		if (line == null) {
			try {
				line = ioBldr.newLine();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		return line; 
	}

	@Override
	public void close() throws IOException {
		writeLine();
		writer.close();
	}
	
	
	@Override
	public boolean isSingleObject() {
		return singleObject;
	}

}
