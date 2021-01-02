package net.sf.cobolToJson.impl.readJson;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;

public class ToJRecordLine extends AFieldsToJRecLine {
	private final AbstractLine line;
	
	
	public ToJRecordLine(AbstractLine line) {
		super();
		this.line = line;
	}

	@Override
	public void endObject(int level, String fieldName) {
	}

	@Override
	public void close() throws IOException {
	}

	@Override
	public AbstractLine getLine() {
		return line;
	}


	@Override
	public boolean isSingleObject() {
		return true;
	}

}
