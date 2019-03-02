package net.sf.JRecord.x.cobol.copyReplacing;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.io.StringReader;

import net.sf.JRecord.Common.Conversion;
import net.sf.cb2xml.CobolPreprocessor;

/**
 * Class to replace strings in a Cobol Copybook. It is a 
 * basic replacement of the COBOL <b>Copy Replacing</b> verb.<br/>
 * Usage:
 * <pre>
 * 
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public class CopyReplacingBldr {

	public static CopyReplacingBldr newBldr(String copybookFileName) throws FileNotFoundException {
		return new CopyReplacingBldr(new FileReader(copybookFileName), CobolColumnDetails.STANDARD_COLUMNS);
	}

	public static CopyReplacingBldr newBldr(String copybookFileName, CobolColumnDetails colDetails) throws FileNotFoundException {
		return new CopyReplacingBldr(new FileReader(copybookFileName), colDetails);
	}
	public static CopyReplacingBldr newBldr(Reader copybookReader, CobolColumnDetails colDetails) {
		return new CopyReplacingBldr(copybookReader, colDetails);
	}

	private StringBuilder copybook;
	
	private CopyReplacingBldr(Reader copybookReader, CobolColumnDetails colDetails) {
		
			copybook = new StringBuilder(
						CobolPreprocessor.preProcess(copybookReader, colDetails.getStartColumn(), colDetails.getEndColumn()));
	}
	
	public CopyReplacingBldr replacing(String from, String to) {
		copybook = Conversion.replace(copybook, from, to);
		return this;
	}
	
	public Reader newReader() {
		return new StringReader(copybook.toString());
	}
	
	public String asString() {
		return copybook.toString();
	}
}
