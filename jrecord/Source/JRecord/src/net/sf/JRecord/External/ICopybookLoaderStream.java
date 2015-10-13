package net.sf.JRecord.External;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Log.AbsSSLogger;

public interface ICopybookLoaderStream extends CopybookLoader {

	/**
	 * Insert a XML Dom Copybook into the Copybook DB
	 *
	 * @param copyBookName Copy Book file Name
	 * @param splitCopybook wether to split a copy book on a redefine / 01
	 * @param dbIdx Database Index
	 * @param font font name to use
	 * @param copybookFormat format of the copybook; see cb2xmlConstants
	 * @param binaryFormat binary format to use
	 * @param systemId System Identifier
	 * @param log log where any messages should be written
	 *
	 * @return return the record that has been read in
	 */
	public abstract ExternalRecord loadCopyBook(
			InputStream inputStream, //Document copyBookXml,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException;
	
}