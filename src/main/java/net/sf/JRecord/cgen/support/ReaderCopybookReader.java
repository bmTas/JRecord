package net.sf.JRecord.cgen.support;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ICobolCopybookTextSource;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

/**
 * This is a low Memory foot-print CopybookReader. It will
 * read the file when the <b>getFreeFormatCopybookReader</b> is called
 * and reread the file each time it is called. It is intended for use
 * with the  <b>ConversionManager</b> classes generated in CodeGen-Message processing.
 * The  <b>ConversionManager</b> will only call <b>getFreeFormatCopybookReader</b> 
 * if the ConversionClass does not exist.
 * 
 * @author Bruce Martin
 *
 */
public class ReaderCopybookReader implements ICobolCopybookTextSource {

	private final String copybookName;
	private final Reader copybookFileReader;
//	private int firstColumn = 0;
	private String freeformatText = null;
	
	private CopybookColumns copybookColumns = CopybookColumns.STANDARD_COLUMNS;
	
	/**
	 * This is a low Memory foot-print CopybookReader. It will
	 * read the file when the <b>getFreeFormatCopybookReader</b> is called
	 * and reread the file each time it is called. It is intended for use
	 * with the  <b>ConversionManager</b> classes generated in CodeGen-Message processing.
	 * 
	 * @param copybookFileName Cobol copybook file name
	 */
	public ReaderCopybookReader(Reader copybookFileReader, String copybookName) {
		super();
		this.copybookFileReader = copybookFileReader;
		this.copybookName = copybookName;
	}

	/**
	 * @param copybookColumns the copybookColumns to set
	 */
	public ReaderCopybookReader setColumns(CopybookColumns copybookColumns) {
		this.copybookColumns = copybookColumns == null ? CopybookColumns.STANDARD_COLUMNS : copybookColumns;
		
		return this;
	}

	@Override
	public String getCopybookName() {
		return copybookName;
	}

	@Override
	public Reader getFreeFormatCopybookReader() {
		StringReader  reader = new StringReader(getFreeFormatText());
		
		return reader;
	}

	@Override
	public int length() {
		return getFreeFormatText().length();
	}
	
	private String getFreeFormatText() {
		if (freeformatText == null) {
			synchronized (this) {
				if (freeformatText == null) {
					try {
						freeformatText = new ReadCobolCopybook()
												.setColumns(copybookColumns)
												.addCobolCopybook(copybookFileReader)
												.getFreeFormatCopybookText();
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}				
			}
		}
		return freeformatText;
	}

	@Override
	public String toPositionMessage(int lineNumber, int columnNumber) {
		return  "Line Number = " + lineNumber + ", Column = " + (columnNumber + copybookColumns.firstColumn);
	}

}
