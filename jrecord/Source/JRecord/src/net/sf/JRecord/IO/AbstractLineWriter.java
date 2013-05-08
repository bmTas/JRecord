/*
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose: Writing Record Orientated files
 */
package net.sf.JRecord.IO;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;


/**
 * This abstract class is the base class for all <b>Line~Writer</b>
 * classes. A LineWriter writes a series of AbstractLines to an external file.
 *
 * <pre>
 * <b>Usage:</b>
 * 
 *         CopybookLoader loader = <font color="brown"><b>new</b></font> RecordEditorXmlLoader();
 *         LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, <font color="brown"><b>null</b></font>).asLayoutDetail();
 *        
 *         <b>AbstractLineWriter</b> writer = LineIOProvider.getInstance().getLineWriter(layout.getFileStructure());
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public abstract class AbstractLineWriter {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";


    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName) throws IOException {
        open(new FileOutputStream(fileName));
    }


    /**
     * Open file for input
     *
     * @param outputStream input stream to write
     *
     * @throws IOException any IOerror
     */
    public abstract void open(OutputStream outputStream)
    throws IOException;


    /**
     * Read one line from the input file
     *
     * @param line line to write to the output file
     *
     * @throws IOException any IOerror
     */
    public abstract void write(AbstractLine line) throws IOException;


    /**
     * Closes the file
     *
     * @throws IOException any IOerror
     */
    public abstract void close() throws IOException;

    /**
     * Set the Record Layout
     * @param layout record layout to set
     */
    public void setLayout(LayoutDetail layout) {

    }
}
