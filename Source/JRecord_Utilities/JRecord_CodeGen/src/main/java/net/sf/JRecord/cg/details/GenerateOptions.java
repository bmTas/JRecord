/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 3 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 3.0 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.details;

import net.sf.JRecord.cg.details.codes.ArgumentOption;
import net.sf.JRecord.cg.schema.CodeGenFileName;
import net.sf.JRecord.cg.schema.LayoutDef;


public class GenerateOptions implements IGenerateOptions {
	
	
	private static final CodeGenFileName EMPTY_FILE_NAME = new CodeGenFileName("");
	


	private  String packageId, packageDir, font, outputDir;
	
	private  ArgumentOption io, splitOption, dialect;
	private  LayoutDef schemaDefinition;
	
	private  boolean dropCopybookName;
	
	private TemplateDtls templateDtls;
	
	
	public GenerateOptions(GenOptValues optionValues) {
		this.packageId = optionValues.packageId;
		this.packageDir = optionValues.packageDir;
		this.font = optionValues.font;
		this.outputDir = optionValues.outputDir;
		
		this.io = optionValues.io; 
		this.splitOption = optionValues.splitOption;
		this.dialect = optionValues.dialect;
		
		this.schemaDefinition = optionValues.schemaDefinition;
		this.dropCopybookName = optionValues.dropCopybookName;
		this.templateDtls = optionValues.templateDtls;
	}
	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getDataFileName()
	 */
	@Override
	public CodeGenFileName getDataFileName() {
		return EMPTY_FILE_NAME;
	}

	@Override
	public final TemplateDtls getTemplateDtls() {
		return templateDtls;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getPackageId()
	 */
	@Override
	public final String getPackageId() {
		return packageId;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getPackageDir()
	 */
	@Override
	public final String getPackageDir() {
		return packageDir;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getFont()
	 */
	@Override
	public final String getFont() {
		return font;
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getOutputDir()
	 */
	@Override
	public final String getOutputDir() {
		return outputDir;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getIo()
	 */
	@Override
	public final ArgumentOption getFileStructureCode() {
		return io;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSplitOption()
	 */
	@Override
	public final ArgumentOption getSplitOption() {
		return splitOption;
	}

	/**
	 * @return the dialect
	 */
	@Override
	public final ArgumentOption getDialect() {
		return dialect;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSchemaDefinition()
	 */
	@Override
	public final LayoutDef getSchemaDefinition() {
		return schemaDefinition;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#isDropCopybookName()
	 */
	@Override
	public final boolean isDropCopybookName() {
		return dropCopybookName;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getConstantValues()
	 */
	@Override
	public ConstantVals getConstantValues() {
		return ConstantVals.CONSTANT_VALUES;
	}
	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getJRecordVersion()
	 */
	@Override
	public int getJRecordVersion() {
		return templateDtls.getJrecordVersion();
	}
   


	@Override
	public String getDefinitionPackageId() {
		return DEFAULT_DEFINITION_PACKAGE_ID;
	}


	@Override
	public String getJRecordPackageId(String id) {
		return getJRecPackageId(id, getJRecordVersion());
	}   

	public static String getJRecPackageId(String id, int version) {
		if (id.startsWith("cgen.impl")  && version <= 101) {
			id = "cgen.impl";
		}
		return id;
	}   

}
