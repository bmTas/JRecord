package net.sf.JRecord.ByteIO;

import java.io.IOException;

import net.sf.JRecord.Common.IGetData;


/**
 * Write byte array records to where ever
 * 
 * @author bruce
 *
 */
public interface IByteRecordWriter {

	/**
	 * Read one line from the input file
	 *
	 * @param bytes line to write to the output file
	 *
	 * @throws IOException any IOerror
	 */
	void write(byte[] bytes) throws IOException;

	/**
	 * Closes the file
	 *
	 * @throws IOException any IOerror
	 */
	void close() throws IOException;


	/**
	 * Write from a data source
	 * @param lineData data source
	 * @throws IOException any IoError
	 */
	void write(IGetData lineData) throws IOException;

}