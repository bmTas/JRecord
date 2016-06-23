package net.sf.JRecord.cg.details;

import net.sf.JRecord.cg.schema.CodeGenFileName;
import net.sf.JRecord.cg.schema.LayoutDef;

public interface IGenerateOptions {

	/**
	 * @return the schemaDefinition
	 */
	public abstract LayoutDef getSchemaDefinition();

	
	/**
	 * 
	 * @return name of the Data file (if known
	 */
	public abstract CodeGenFileName getDataFileName();

	
	/**
	 * @return the template
	 */
	public abstract TemplateDtls getTemplateDtls();


	/**
	 * @return the packageId
	 */
	public abstract String getPackageId();

	/**
	 * @return the packageDir
	 */
	public abstract String getPackageDir();

	/**
	 * @return the font
	 */
	public abstract String getFont();
	
	/**
	 * @return the outputDir
	 */
	public abstract String getOutputDir();

	/**
	 * @return the io
	 */
	public abstract ArgumentOption getFileStructureCode();

	/**
	 * @return the splitOption
	 */
	public abstract ArgumentOption getSplitOption();

	public abstract ArgumentOption getDialect();

	/**
	 * @return the dropCopybookName
	 */
	public abstract boolean isDropCopybookName();
	
	public abstract ConstantVals getConstantValues();


}