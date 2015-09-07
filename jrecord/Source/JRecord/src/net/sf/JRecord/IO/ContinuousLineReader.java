/*
 * Purpose: Record oriented reading of Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Started work on seperating Record section out, so removing
 *     all reference to the Common module and used a new Constants
 *     module
 */
package net.sf.JRecord.IO;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;


/**
 * This class provides record oriented reading of a binary File
 * using the <b>Line</b> class to decide the record length.
 *
 * @author Bruce Martin
 *
 */
public class ContinuousLineReader extends AbstractLineReader {


	private static final int BUFFER_SIZE = 16384;

 	private BufferedInputStream stream = null;
	//private LayoutDetail recordLayout;

	private int maxSize;

	private byte[] buffer;

	protected int[] lengths;
	private AbstractLine tmpLine;

	//private ArrayList lineBuffer = new ArrayList();

//	private int lineNumber = 0;



	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record
	 */
	public ContinuousLineReader() {
	    super();
	}


	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record.
	 * It also uses a Line-Provider to create the lines.
	 *
	 * @param provider line provider
	 */
	public ContinuousLineReader(final LineProvider provider) {
	    super(provider);
	}


    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout) {

        int i;

        stream = new BufferedInputStream(inputStream, BUFFER_SIZE);
        setLayout(layout);

		lengths = new int[layout.getRecordCount()];

		maxSize = 0;

		for (i = 0; i < lengths.length; i++) {
			lengths[i] = layout.getRecord(i).getLength();

			maxSize = java.lang.Math.max(maxSize, lengths[i]);
		}

		tmpLine = getLine(new byte[maxSize]);

		//lineBuffer.clear();

		buffer = new byte[maxSize];
    }



    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#read()
     */
    public AbstractLine read() throws IOException {
        AbstractLine ret = null;
        int recordSize, bytesRead;
        byte[] rec;

        if (stream == null) {
            throw new IOException(AbstractLineReader.NOT_OPEN_MESSAGE);
        }

        stream.mark(maxSize);
        bytesRead = readBuffer(stream, buffer);

        if (bytesRead <= 0) {
            return null;
        }
        if (bytesRead < buffer.length) {
            bytesRead += 1;
        }
        tmpLine.replace(buffer, 0, bytesRead);
        recordSize = findLength(tmpLine, bytesRead);
        stream.reset();
        rec = new byte[recordSize];

        readBuffer(stream, rec);
        ret = getLine(rec);


        return ret;
    }


	/**
	 * work out the record length
	 *
	 * @param maxLength length of the buffer
	 *
	 * @return the length of the next length
	 */
	protected int findLength(AbstractLine tmpLine, int maxLength) {
		int pref = tmpLine.getPreferredLayoutIdxAlt();
		
		if (pref < 0) {
		    throw new RuntimeException("Can Not Determine Record Type: " + tmpLine.getFullLine());
		}

		return lengths[pref];
	}


    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#close()
     */
    public void close() throws IOException {

        stream.close();
        buffer = null;
        stream = null;
    }

}
