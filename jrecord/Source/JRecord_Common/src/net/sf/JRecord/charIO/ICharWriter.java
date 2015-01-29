package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Defines a class that will read a logical record from a file
 * as a String
 * 
 * @author Bruce Martin
 *
 */
public interface ICharWriter {

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
	public abstract void open(OutputStream outputStream) throws IOException;

	/**
	 * Write one line to the output file
	 *
	 * @throws IOException io error
	 */
	public abstract void write(String line) throws IOException;

	/**
	 * Write one line to the output file
	 *
	 * @throws IOException io error
	 */
	public abstract void write(char[] line) throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException io error
	 */
	public abstract void close() throws IOException;
}