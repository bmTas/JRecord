package net.sf.JRecord.Common.layoutSupport;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;

public interface ISchemaSummary {

	/**
	 * Get a specified record
	 * @param recordIndex Index of required record
	 * @return requested record
	 */
	ISchemaRecord getRecord(int recordIndex);

	/**
	 * Get record count
	 * @return record count
	 */
	int getRecordCount();

	/**
	 * @return the fontName
	 */
	String getFontName();

	/**
	 * @return the fileStructure. The various file structure's are constants in
	 * interface IFileStructureConstants. i.e. IFileStructureConstants.IO_STANDARD_TEXT_FILE
	 */
	int getFileStructure();


}