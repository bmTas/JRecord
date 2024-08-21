package net.sf.JRecord.x.cobol.randomAccess;

import java.io.IOException;
import java.io.RandomAccessFile;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

/**
 * Example of reading a fixed length file randomly.
 * The record position is calculated as 
 *    recordPosition = recordLength * RecordNumber; // first record-number = 0;  
 * @author Bruce Martn
 *
 */
public class RandomLineReader {

	private final int recordLength;
	private final RandomAccessFile raFile;
	private final ISchemaIOBuilder ioBuilder;
	public RandomLineReader(String fileName, ISchemaIOBuilder ioBuilder) throws IOException {
		this.recordLength = ioBuilder.getLayout().getMaximumRecordLength();
		this.raFile = new RandomAccessFile(fileName, "r");
		this.ioBuilder = ioBuilder;
	}

	/**
	 * get a specified record number. The first record is record 0
	 * 
	 * @param recordNumber record number to retrieve
	 * @return requested line
	 * 
	 * @throws IOException
	 */
	public AbstractLine read(int recordNumber) throws IOException {
		long pos = ((long)recordLength) * recordNumber;
		byte[] bytes = new byte[recordLength];

		raFile.seek(pos);
		raFile.readFully(bytes);
		
		return ioBuilder.newLine(bytes);
	}

	public void close() throws IOException {
		raFile.close();
	}
}
