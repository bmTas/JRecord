package net.sf.JRecord.cgen.def;

public interface IEncoder<T> {
	public abstract byte[] encode(T rec);
}
