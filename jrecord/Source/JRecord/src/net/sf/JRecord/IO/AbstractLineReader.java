/*
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose:  reading Record Orientated files
 */
package net.sf.JRecord.IO;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.ExternalRecord;



/**
 * This abstract class is the base class for all <b>Line~Reader</b>
 * classes. A LineReader reads a file as a series of AbstractLines.
 *
 * <pre>
 * <b>Usage:</b>
 * 
 *         CopybookLoader loader = <font color="brown"><b>new</b></font> RecordEditorXmlLoader();
 *         LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, <font color="brown"><b>null</b></font>).asLayoutDetail();
 *        
 *         <b>AbstractLineReader</b> reader = LineIOProvider.getInstance().getLineReader(layout.getFileStructure());
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public abstract class AbstractLineReader {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";

	private LineProvider lineProvider;
	private LayoutDetail layout = null;


	/**
	 * create Binary Line Reader
	 */
	public AbstractLineReader() {
	    this(new DefaultLineProvider());
	}


	/**
	 * create reader with a user sup[plied line provider. This allows
	 * you to use your own Classes that extend "Line"
	 *
	 * @param provider line provider
	 */
	public AbstractLineReader(final LineProvider provider) {
	    super();
	    lineProvider = provider;

	    if (provider == null) {
	    	lineProvider = new DefaultLineProvider();
	    }
	}

	/**
	 * Open a file where the layout can be built from the file contents.
	 * Possible files are:
	 * - XML files
	 * - CSV files where the field names are stored on the first line
	 * @param fileName file to be opened.
	 */
	 public void open(String fileName) throws IOException, RecordException {
		 open(fileName, (LayoutDetail) null);
	 }
    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     * @param pLayout record layout
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName, LayoutDetail pLayout) throws IOException, RecordException {
        open(new FileInputStream(fileName), pLayout);

        if (layout == null) {
            layout = pLayout;
        }
    }
    

    
    /**
     * Open a file using an external record Definition 
     * @param inputStream input
     * @param recordLayout recordlayout to use
     * @throws IOException any IOError that occurs
     * @throws RecordException any other error
     */
    public void open(InputStream inputStream, ExternalRecord recordLayout) 
    throws IOException, RecordException {
    	LayoutDetail pLayout = recordLayout.asLayoutDetail();
    	open(inputStream, pLayout);

        if (layout == null) {
            layout = pLayout;
        }
    }


    /**
     * Open file for input
     *
     * @param inputStream input stream to be read
     * @param pLayout record layout
     *
     * @throws IOException any IOerror
     */
    public abstract void open(InputStream inputStream, LayoutDetail pLayout)
    throws IOException, RecordException;



    /**
     * Read one line from the input file
     *
     * @return line read in
     *
     * @throws IOException io error
     */
    public abstract AbstractLine read() throws IOException;


    /**
     * Closes the file
     *
     * @throws IOException io error
     */
    public abstract void close() throws IOException;



	/**
	 * Read a complete buffers worth of data into buf from a input stream.
	 *
	 * @param in stream to be read.
	 * @param buf buffer to be loaded with data
	 *
	 * @return the number of bytes read
	 * @throws IOException IO Exception
	 */
	protected final int readBuffer(final InputStream in,
	        					   final byte[] buf)
				throws IOException {
	    int num;
	    int total;

	    num = in.read(buf);
	    total = num;

	    while (num >= 0 && total < buf.length) {
	        num = in.read(buf, total, buf.length - total);
	        total += num;
	    }

	    return total;
	}


	/**
	 * Create a Line using supplied details
	 *
	 * @param record record contents
	 *
	 * @return line just created
	 */
	protected final AbstractLine getLine(byte[] record) {
	    AbstractLine ret = lineProvider.getLine(layout, record);

	    ret.setLineProvider(lineProvider);
	    return ret;
	}


	/**
	 * Create a Line using supplied details
	 *
	 * @param record record contents
	 *
	 * @return line just created
	 */
	protected final AbstractLine getLine(String record) {
	    AbstractLine ret = lineProvider.getLine(layout, record);

	    ret.setLineProvider(lineProvider);
	    return ret;
	}


	/**
	 * get the record layout
	 * @return the layout
	 */
	public final LayoutDetail getLayout() {
	    return layout;
	}

	/**
	 * set the layout to be used
	 *
	 * @param pLayout layout to be used
	 */
    public final void setLayout(LayoutDetail pLayout) {
        this.layout = pLayout;
    }
}
