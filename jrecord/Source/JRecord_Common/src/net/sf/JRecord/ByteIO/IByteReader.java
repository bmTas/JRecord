package net.sf.JRecord.ByteIO;

import java.io.IOException;
import java.io.InputStream;

public interface IByteReader {

	/**
	 * Open file for input
	 *
	 * @param fileName filename to be opened
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(String fileName) throws IOException;

	/**
	 * Open file for input
	 *
	 * @param inputStream input stream to be read
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(InputStream inputStream) throws IOException;

	/**
	 * Read one line from the input file
	 *
	 * @return line read in
	 *
	 * @throws IOException io error
	 */
	public abstract byte[] read() throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException io error
	 */
	public abstract void close() throws IOException;

	/**
	 * @param lineLength The lineLength to set.
	 */
	public abstract void setLineLength(int lineLength);

	public abstract boolean canWrite();

	public abstract long getBytesRead();

}