package net.sf.JRecord.IO;

import net.sf.JRecord.Details.AbstractLine;

public interface ICalculateLineLength {

	/**
	 * work out the record length
	 *
	 * @param maxLength length of the buffer
	 *
	 * @return the length of the next length
	 */
	int findLength(AbstractLine tmpLine, int maxLength);

	/**
	 * Set the file position
	 * @param lineNumber
	 * @param totalBytesRead
	 */
	void setFilePosition(int lineNumber, long totalBytesRead);

}