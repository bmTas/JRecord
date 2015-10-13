package net.sf.JRecord.cgen.def;

public interface IDecoder<T> {
	public abstract T decode(byte[] rec);
}
