package net.sf.JRecord.charIO;

import java.io.IOException;
import java.io.InputStream;

public interface IOpenInputStream {
	/**
	 * Open file for input
	 *
	 * @param inputStream input stream to be read
	 *
	 * @throws IOException any IOerror
	 */
	public abstract ICharReader open(InputStream inputStream, String font) throws IOException;


}
