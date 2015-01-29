/*
 * @Author Bruce Martin
 * Created on 19/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.Details.AbstractLine;

/**
 * This class creates a <b>LineWriter</b> from a Low Level <b>Byte-Writer</b>.
 * A ByteWriter writes a Byte Array to a File as a Line
 *
 * There a Byte Writers for <ol compact>
 *   <li>Fixed Line Length files
 *   <li>Length Based lines
 * </ol>
 *
 * @author Bruce Martin
 *
 */
public class LineWriterWrapper extends AbstractLineWriter {

    private AbstractByteWriter writer;

    /**
     *
     */
    public LineWriterWrapper(AbstractByteWriter byteWriter) {
        super();

        writer = byteWriter;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {
        writer.open(outputStream);
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#write(net.sf.JRecord.Details.AbstractLine)
     */
    public void write(AbstractLine line) throws IOException {
        writer.write(line.getData());
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {
    	if (writer != null) {
    		writer.close();
    	}
    }

	/**
	 * @param writer the writer to set
	 */
	public final void setWriter(AbstractByteWriter writer) {
		this.writer = writer;
	}
}
