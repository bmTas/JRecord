package net.sf.JRecord.cgen.def;

import java.io.IOException;

public interface IReader<T> {
	public T read() throws IOException;
	public void close() throws IOException;
}
