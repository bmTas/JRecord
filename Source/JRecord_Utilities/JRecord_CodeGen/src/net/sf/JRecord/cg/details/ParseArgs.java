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

import net.sf.JRecord.utilityClasses.ParseArguments;

public class ParseArgs extends ParseArguments   {
	private static String[] HELP_ARG = {"-h"};
	private static final String[] SINGLE_ARGS = {
		ArgumentOption.OPT_SCHEMA, ArgumentOption.OPT_PACKAGE, ArgumentOption.OPT_TEMPLATE, 
		ArgumentOption.OPT_TEMPLATE_DIRECTORY,
		ArgumentOption.OPT_DIALECT,
		ArgumentOption.OPT_FILE_ORGANISATION,
		ArgumentOption.OPT_LOAD_SCHEMA, "-h", "-?", "-help", ArgumentOption.OPT_SPLIT,
		ArgumentOption.OPT_FONT_NAME, ArgumentOption.OPT_DROP_COPYBOOK_NAME, ArgumentOption.OPT_OUTPUT_DIR,
		ArgumentOption.OPT_GEN_DATE,
		ArgumentOption.OPT_JREC_VERSION,
	};
	private static final String[] MULTI_ARGS = {
		ArgumentOption.OPT_RECSEL, ArgumentOption.OPT_GENERATE
	};
	
	public ParseArgs(String... args) {
		super(SINGLE_ARGS, MULTI_ARGS, args==null||args.length==0 ?HELP_ARG :args);
	}
}
