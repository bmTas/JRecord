package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.InputStream;

/**
 * Defines a class that will read a logical record from a file
 * as a String
 * 
 * @author Bruce Martin
 *
 */
public interface ICharReader {

	/**
	 * Open file for input
	 *
	 * @param fileName filename to be opened
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(String fileName, String font) throws IOException;

	/**
	 * Open file for input
	 *
	 * @param inputStream input stream to be read
	 *
	 * @throws IOException any IOerror
	 */
	public abstract void open(InputStream inputStream, String font) throws IOException;

	/**
	 * Read one line from the input file
	 *
	 * @return line read in
	 *
	 * @throws IOException io error
	 */
	public abstract String read() throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException io error
	 */
	public abstract void close() throws IOException;
}