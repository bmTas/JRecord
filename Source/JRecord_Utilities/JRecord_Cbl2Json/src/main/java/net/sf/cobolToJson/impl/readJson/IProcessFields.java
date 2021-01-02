package net.sf.cobolToJson.impl.readJson;

import java.io.IOException;

public interface IProcessFields {
	void endObject(int level, String fieldName);
	void updateField(String fieldName, long value);
	void updateField(String fieldName, String value);
	void close() throws IOException;
	boolean isSingleObject();
}
