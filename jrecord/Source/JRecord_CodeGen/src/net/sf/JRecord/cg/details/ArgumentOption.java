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

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;

public class ArgumentOption {
	public static final String OPT_SCHEMA = "-Schema";
	public static final String OPT_TEMPLATE_DIRECTORY = "-TemplateDirectory";
	public static final String OPT_TEMPLATE = "-Template";
	public static final String OPT_SPLIT = "-split";
	public static final String OPT_GENERATE = "-Generate";
	public static final String OPT_FILE_ORGANISATION = "-FileOrganisation";
	public static final String OPT_LOAD_SCHEMA = "-loadSchema";
	public static final String OPT_PACKAGE = "-package";
	
	public static final String OPT_DROP_COPYBOOK_NAME = "-DropCopybookName";
	public static final String OPT_FONT_NAME = "-font";
	
	public static final String OPT_OUTPUT_DIR = "-outputDirectory";

	
	public static final String JAVA_POJO_TEMPLATE = "javaPojo";
	public static final ArgumentOption TEMPLATE_JAVA_POJO = stdTemplateArg(JAVA_POJO_TEMPLATE,   "Generate java classes for each Cobol Record");
	public static final ArgumentOption TEMPLATE_BASIC = stdTemplateArg("basic",  "Generate example code using JRecord IO Builders");
	public static final ArgumentOption TEMPLATE_STD_POJO = stdTemplateArg("stdPojo",   "Generate java wrapper classes for each Cobol Record");
	public static final ArgumentOption TEMPLATE_SCHEMA_CLASS = stdTemplateArg("schemaClass", "Generate example code using JRecord IO Builders + Schema details");
	public static final ArgumentOption TEMPLATE_LINE_WRAPPER = stdTemplateArg("lineWrapper", "Generate Wrapper classes for JRecord-Lines");
	public static final ArgumentOption TEMPLATE_STANDARD = stdTemplateArg("standard",  "Generate example code using JRecord IO Builders + Field Name Class");

	public static final ArgumentOption[] TEMPLATE_OPTIONS = {
		TEMPLATE_STANDARD,
		TEMPLATE_LINE_WRAPPER,
		TEMPLATE_SCHEMA_CLASS,
		TEMPLATE_STD_POJO,
		TEMPLATE_JAVA_POJO,
		TEMPLATE_BASIC,
	};


	public static final ArgumentOption[] SPLIT_OPTS = {
		new ArgumentOption("None",      "CopybookLoader.SPLIT_NONE", "No Copybook split", CopybookLoader.SPLIT_NONE ),
		new ArgumentOption("01",        "CopybookLoader.SPLIT_01_LEVEL", "Split on 01", CopybookLoader.SPLIT_01_LEVEL),
		new ArgumentOption("redefine",  "CopybookLoader.SPLIT_REDEFINE", "Split on redefine", CopybookLoader.SPLIT_REDEFINE),
		new ArgumentOption("highest",   "CopybookLoader.SPLIT_HIGHEST_REPEATING", "Hightest Level", CopybookLoader.SPLIT_HIGHEST_REPEATING),
	};

	public static final ArgumentOption[] FILE_ORGANISATION_OPTS = {
		new ArgumentOption("Text",         "Constants.IO_BIN_TEXT",     "Standard Windows/Unix text file (single byte characterset)", Constants.IO_BIN_TEXT),
		new ArgumentOption("FixedWidth",   "Constants.IO_FIXED_LENGTH", "File where lines (records) are the same length no \\n", Constants.IO_FIXED_LENGTH),
		new ArgumentOption("Mainframe_VB", "Constants.IO_VB",           "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB)};

	
	public final String option, code, description;
	public final int id;
	
	public static ArgumentOption stdTemplateArg(String option, String description) {
		return new ArgumentOption(option, option, description, 0);
	}
	public ArgumentOption(String option, String code, String description) {
		this(option, code, description, 0);
	}
	public ArgumentOption(String option, String code, String description, int id) {
		super();
		this.option = option;
		this.code = code;
		this.description = description;
		this.id = id;
	}
	
	/**
	 * @return the option
	 */
	public final String getOption() {
		return option;
	}
	
	/**
	 * @return the code
	 */
	public final String getCode() {
		return code;
	}
	
	/**
	 * @return the description
	 */
	public final String getDescription() {
		return description;
	}
	
	/**
	 * @return the id
	 */
	public final int getId() {
		return id;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return option;
	}

	
}
