package net.sf.JRecord.cgen.def;

import java.io.IOException;

public interface IWriter<T> {
	public abstract void write(T record) throws IOException;
	public void close() throws IOException;

}
