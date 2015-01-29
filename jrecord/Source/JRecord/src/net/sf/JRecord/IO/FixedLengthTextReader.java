package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.charIO.FixedLengthCharReader;

/**
 * Fixed Length Character file line reader.
 * A reader for a file where all the records are the same length
 * 
 * @author Bruce Martin
 *
 */
public class FixedLengthTextReader extends BasicTextLineReader {

	public FixedLengthTextReader(LineProvider provider) {
		super(provider);
	}


    /**
     * @see net.sf.JRecord.IO.StandardLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout)
    throws IOException, RecordException {
    	String font = "";
		if (layout != null) {
			font = layout.getFontName();
		}
    	
    	super.open(new FixedLengthCharReader(layout.getMaximumRecordLength()), inputStream, layout, font);
    }


}
