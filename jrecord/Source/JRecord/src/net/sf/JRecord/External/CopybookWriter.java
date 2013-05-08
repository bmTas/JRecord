package net.sf.JRecord.External;

import java.io.OutputStream;

import net.sf.JRecord.Log.AbsSSLogger;


/**
 * Description of a Class to write a copybook (Record Layout or Record Description) 
 * to a file
 * 
 * @author Bruce Martin
 *
 */
public interface CopybookWriter {

	/**
	 * Write a copybook to a file
	 * @param directory Directory to write copybooks
	 * @param copybook Copybook Details
	 * @param log log of errors that occur
	 * @throws Exception any error that occurs
	 */
	
	public abstract String writeCopyBook(final String directory, ExternalRecord copybook,
            final AbsSSLogger log) throws Exception;
	
	/**
	 * Write Copybook to a Stream
	 * @param outStream stream where the copybook should be written
	 * @param copybook copybook to be written
	 * @param log log for any errors to be written
	 * @throws Exception any errors thrown
	 */
	public abstract void writeCopyBook(final OutputStream outStream, ExternalRecord copybook,
            final AbsSSLogger log) throws Exception;

}
