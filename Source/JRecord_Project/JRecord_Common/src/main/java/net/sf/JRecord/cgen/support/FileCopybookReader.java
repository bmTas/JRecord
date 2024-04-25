package net.sf.JRecord.cgen.support;

import java.io.File;
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
public class FileCopybookReader implements ICobolCopybookTextSource {

	private final String copybookName;
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
	public FileCopybookReader(String copybookFileName) {
		super();
		this.copybookName = copybookFileName;
	}

	/**
	 * @param copybookColumns the copybookColumns to set
	 */
	public FileCopybookReader setColumns(CopybookColumns copybookColumns) {
		this.copybookColumns = copybookColumns == null ? CopybookColumns.STANDARD_COLUMNS : copybookColumns;
		
		return this;
	}

	@Override
	public String getCopybookName() {
		return new File(copybookName).getName();
	}

	@Override
	public Reader getFreeFormatCopybookReader() {
		StringReader  reader = new StringReader(getFreeFormatText());
		
		freeformatText = null;
		return reader;
	}

	@Override
	public int length() {
		return getFreeFormatText().length();
	}
	
	private String getFreeFormatText() {
		String tempCopybookText = freeformatText;
		if (tempCopybookText == null) {
			synchronized (this) {
				tempCopybookText = freeformatText;
				if (tempCopybookText == null) {
					try {
						tempCopybookText = new ReadCobolCopybook()
												.setColumns(copybookColumns)
												.addCobolCopybook(copybookName)
												.getFreeFormatCopybookText();
						freeformatText = tempCopybookText;
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
		return tempCopybookText;
	}

	@Override
	public String toPositionMessage(int lineNumber, int columnNumber) {
		return  "Line Number = " + lineNumber + ", Column = " + (columnNumber + copybookColumns.firstColumn);
	}

}
