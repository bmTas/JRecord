package net.sf.JRecord.External;

import net.sf.JRecord.Common.BasicNamedManager;

/**
 * Class to manage all the Copybook Writer class's
 *
 * @author Bruce Martin
 *
 */
public class CopybookWriterManager extends BasicNamedManager<CopybookWriter> {

	public static final int RECORD_EDITOR_TAB_CSV_WRITER = 0;
	public static final int RECORD_EDITOR_XML_WRITER = 1;

	private static final int NUMBER_OF_WRITERS = 5;
	private static CopybookWriterManager instance = null;



	public CopybookWriterManager() {
		super("Copybook_Writers", NUMBER_OF_WRITERS, NUMBER_OF_WRITERS, new CopybookWriter[NUMBER_OF_WRITERS]);
		register(0, "RecordEditor (Tab) CSV", new RecordEditorCSVWriter("\t"));
		register(1, "RecordEditor XML", new RecordEditorXmlWriter());
//		ExternalConversion.setStandardConversion(this);
	}


	/**
	 * @return the instance
	 */
	public static CopybookWriterManager getInstance() {
		if (instance == null) {
			instance = new CopybookWriterManager();
		}
		return instance;
	}
}
