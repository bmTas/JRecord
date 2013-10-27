package net.sf.JRecord.ByteIO;

interface IBufferDetails {

	public abstract byte[] getBuffer();

	public abstract boolean checkFor(int pos, byte[] search);

	public abstract int[] getLineArray();

	public abstract int getNoLines();

	public abstract int incNoLines();

}