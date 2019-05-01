package net.sf.JRecord.cg.details;

import net.sf.JRecord.cg.schema.CodeGenFileName;
import net.sf.JRecord.cg.schema.LayoutDef;

/**
 * This interface is passed to every "Velocity" Template
 * (as variable generateOptions). It contains all the details
 * need to generate the skelton
 * 
 * @author Bruce Martin
 *
 */
public interface IGenerateOptions {

	public static String DEFAULT_DEFINITION_PACKAGE_ID = "def";
	/**
	 * Get the 'Extended' Layout (or file schema definition)
	 * @return the schemaDefinition
	 */
	public abstract LayoutDef getSchemaDefinition();

	
	/**
	 * Get the Cobol data file name
	 * 
	 * @return name of the Data file (if known
	 */
	public abstract CodeGenFileName getDataFileName();

	
	/**
	 * Get the Template Details
	 * @return the template
	 */
	public abstract TemplateDtls getTemplateDtls();


	/**
	 * get the Java package id
	 * 
	 * @return the packageId
	 */
	public abstract String getPackageId();

	/**
	 * Get the package directory
	 * 
	 * @return the package Directory
	 */
	public abstract String getPackageDir();

	/**
	 * Get the Font (character-set) of the Layout (file schema)
	 * @return the font
	 */
	public abstract String getFont();
	
	/**
	 * Get the Sample Output file (for use in the Write class).
	 * 
	 * @return the outputDir
	 */
	public abstract String getOutputDir();

	/**
	 * Get the JRecord File-Structure (File-Organisation)
	 * is various representations
	 * @return the File-Structure
	 */
	public abstract ArgumentOption getFileStructureCode();

	/**
	 * Get the Cobol-Split option
	 * 
	 * @return the  Cobol-Split option
	 */
	public abstract ArgumentOption getSplitOption();

	/**
	 * Get the Cobol-Dialect (e.g. Mainframe, GNU-Cobol) 
	 * @return the Cobol-Dialect (e.g. Mainframe, GNU-Cobol) 
	 */
	public abstract ArgumentOption getDialect();

	/**
	 * @return the dropCopybookName
	 */
	public abstract boolean isDropCopybookName();
	
	
	/**
	 * Get the Record-Selection boolean options
	 * 
	 * @return Record-Selection boolean options
	 */
	public abstract ConstantVals getConstantValues();

	/**
	 * get the Schema Definition Package Id.
	 * In version 0.90 of JRecord 
	 */
	public abstract String getDefinitionPackageId();
	
	/**
	 * JRecord version to generate for
	 * @return
	 */
	public abstract int getJRecordVersion();


}