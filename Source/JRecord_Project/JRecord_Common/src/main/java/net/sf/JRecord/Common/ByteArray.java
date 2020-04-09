package net.sf.JRecord.Common;

public class ByteArray {

	private byte[] bytes;
	int size=0;
	
	public ByteArray() {
		bytes = new byte[10];
	}

	public ByteArray(int size) {
		bytes = new byte[size];
	}
	public ByteArray add(byte b) {
		checkSize(size + 1);
		bytes[size++] = b;
		return this;
	}

	public ByteArray add(byte[] b) {
		checkSize(size + b.length);
		System.arraycopy(b, 0, bytes, size, b.length);
		size += b.length;
		
		return this;
	}
	
	public void clear() {
		size = 0;
	}
	
	public int length() {
		return size;
	}

	protected void checkSize(int newArraySize) {
		if (bytes.length < newArraySize) {
			int l = newArraySize + Math.max(5, newArraySize / 5);
			byte[] tmp = new byte[l];
			System.arraycopy(bytes, 0, tmp, 0, bytes.length);
			bytes = tmp;
		}
	}
	public byte[] toByteArray() {
		byte[] ret = new byte[size];
		System.arraycopy(bytes, 0, ret, 0, size);
		return ret;
	}
}
