package net.sf.JRecord.cg.details;

import net.sf.JRecord.cg.schema.LayoutDef;

public interface IGenerateOptions {

	/**
	 * @return the schemaDefinition
	 */
	public abstract LayoutDef getSchemaDefinition();

	
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
	public abstract ArgumentOption getIo();

	/**
	 * @return the splitOption
	 */
	public abstract ArgumentOption getSplitOption();

	/**
	 * @return the dropCopybookName
	 */
	public abstract boolean isDropCopybookName();

}