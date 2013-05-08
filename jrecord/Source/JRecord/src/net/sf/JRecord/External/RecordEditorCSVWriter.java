package net.sf.JRecord.External;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.AbsSSLogger;

/**
 * Write a RecordLayout as a RecordEditor-CSBV layout file.
 * Basically The first record holds Record Details
 * and each line holds a field definition:
 * 
 * <ol>
 * <li>Starting Position
 * <li>Length
 * <li>Number of places after the decimal point
 * <li>Field Name
 * <li>Field Type (String [or char], num, mainframe_zoned, fuji_zoned)
 * <li>Decimal - number of places after the decimal point
 * <li>Format Field Format (used in the Record Editor)
 * <li>Parameter - parameter for the Type / Format
 * </ol>
 * 
 * @author Bruce Martin
 *
 */
public class RecordEditorCSVWriter implements CopybookWriter {

	private String fldSeperator = "\t"; 
	
	/**
	 * Create Record Editor CSV Writer
	 * @param fieldSeperator field Seperator (i.e. Tab or Comma, etc)
	 */
	public RecordEditorCSVWriter(String fieldSeperator) {
		fldSeperator = fieldSeperator;
	}
	
	
	
	/**
	 * @see net.sf.JRecord.External.CopybookWriter#writeCopyBook(java.lang.String, net.sf.JRecord.External.ExternalRecord, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public String writeCopyBook(String directory, ExternalRecord copybook,
			AbsSSLogger log) throws Exception {
		String fileName;
		directory = ExternalConversion.fixDirectory(directory);

		for (int i = 0; i < copybook.getNumberOfRecords(); i++) {
			writeCopyBook(directory, copybook.getRecord(i), log);
		}
		
		fileName = directory + copybook.getRecordName() + Constants.TXT_EXTENSION;
		copybook.setRecordName(ExternalConversion.copybookNameToFileName(copybook.getRecordName()));
		writeCopyBook(new FileOutputStream(fileName), 
				copybook, log);
		return fileName;
	}

	/**
	 * @see net.sf.JRecord.External.CopybookWriter#writeCopyBook(java.io.OutputStream, net.sf.JRecord.External.ExternalRecord, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public void writeCopyBook(OutputStream outStream, ExternalRecord copybook,
			AbsSSLogger log) throws Exception {

		int i;
		
//		if (copybook.getNumberOfRecordFields()> 0) {
		ExternalField field;
		ExternalRecord sr;
		String description;
		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(outStream));
		
		writer.write(Constants.RECORD_NAME + fldSeperator
				+ copybook.getFileStructure() + fldSeperator
				+ copybook.getRecordType() + fldSeperator
				+ copybook.getDelimiter() + fldSeperator
				+ copybook.getRecordStyle() + fldSeperator
				+ copybook.getQuote() + fldSeperator
				+ copybook.getListChar() + fldSeperator
				+ fixDescription(copybook.getDescription())
		);
		writer.newLine();

		for (i = 0; i < copybook.getNumberOfRecords(); i++) {
			sr = copybook.getRecord(i);
			writer.write(Constants.SUB_RECORD_NAME + fldSeperator
					+ sr.getRecordName() + fldSeperator
					+ sr.getTstField() + fldSeperator
					+ sr.getTstFieldValue() + fldSeperator
					+ sr.getParentRecord() + fldSeperator
					+ sr.getListChar()
			);
			writer.newLine();
		}

		for (i = 0; i < copybook.getNumberOfRecordFields(); i++) {
			field = copybook.getRecordField(i);
			description = field.getDescription();
			if (description == null) {
				description = "";
			} else if (description.indexOf(fldSeperator) >= 0
					||  description.indexOf('\n') >= 0) {
				description = fixDescription(description);
			}
			writer.write(field.getPos() + fldSeperator
					+ field.getLen() + fldSeperator
					+ field.getName() + fldSeperator
					+ description + fldSeperator
					+ field.getType() + fldSeperator
					+ field.getDecimal() + fldSeperator
					+ field.getCellFormat() + fldSeperator
					+ field.getParameter());
			writer.newLine();
		}
		writer.close();
		outStream.close();
//		}
	}
	
	private String fixDescription(String description) {	
		StringBuilder b = new StringBuilder(description);
		
		Conversion.replace(b,  fldSeperator, " ");
		Conversion.replace(b,  "\n", "\\n");
		return b.toString();
	}

}
