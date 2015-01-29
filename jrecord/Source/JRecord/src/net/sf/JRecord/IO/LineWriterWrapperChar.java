/*
 * @Author Bruce Martin
 * Created on 19/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.charIO.CharIOProvider;
import net.sf.JRecord.charIO.ICharWriter;

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
public class LineWriterWrapperChar extends AbstractLineWriter {

	private static final CharIOProvider ioProvider = new CharIOProvider();
	
    private ICharWriter writer = null;
    private final int fileStructure;
    private OutputStream outputStream = null;

    /**
     *
     */
    public LineWriterWrapperChar(int fileStructure) {
        super();
        this.fileStructure = fileStructure;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {
    	this.outputStream = outputStream;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#write(net.sf.JRecord.Details.AbstractLine)
     */
    public void write(AbstractLine line) throws IOException {
    	if (writer == null) {
    		LayoutDetail layout = line.getLayout();
			writer = ioProvider.getWriter(
					fileStructure, 
					layout.getFontName(), layout.getEolString(), layout.getMaximumRecordLength());
    		writer.open(outputStream);
    	}
        writer.write(line.getFullLine());
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {
    	if (writer != null) {
    		writer.close();
    	} 
    	if (outputStream != null) {
    		outputStream.close();
    	}
    	writer = null;
    	outputStream = null;
    }
}
