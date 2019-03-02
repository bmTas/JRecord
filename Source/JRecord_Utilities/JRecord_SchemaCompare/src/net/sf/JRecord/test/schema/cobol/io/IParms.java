package net.sf.JRecord.test.schema.cobol.io;

/**
 * Parameters For either generating the "Compare" file or
 * doing the compare
 * 
 * @author Bruce Martin
 *
 */
public interface IParms {

//	/**
//	 * @return the directory
//	 */
//	public abstract String getDirectory();

	/**
	 * @return the font
	 */
	public abstract String getFont();

	/**
	 * @return the outputFileName
	 */
	public abstract String getOutputFileName();

	/**
	 * @return the dialect
	 */
	public abstract String getInputFileName();

	/**
	 * @return the file-structure
	 */
	public abstract int getDialect();

	/**
	 * @return the inputFileName
	 */
	public abstract int getFileStructure();

	/**
	 * @return the dropCopybookName
	 */
	public abstract boolean isDropCopybookName();

	public abstract String[] getFileNames();

}