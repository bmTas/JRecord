/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.details;

import net.sf.JRecord.utilityClasses.ParseArguments;

public class ParseArgs extends ParseArguments implements ArgNames {
	
	private static final String[] SINGLE_ARGS = {
		OPT_SCHEMA, ArgNames.OPT_PACKAGE, OPT_TEMPLATE, 
		OPT_FILE_ORGANISATION,
		OPT_LOAD_SCHEMA, "-packageId", "-h", "-?", "-help", OPT_SPLIT,
		OPT_FONT_NAME, OPT_DROP_COPYBOOK_NAME, OPT_OUTPUT_DIR
	};
	private static final String[] MULTI_ARGS = {
		OPT_GENERATE
	};
	
	public ParseArgs(String... args) {
		super(SINGLE_ARGS, MULTI_ARGS, args);
	}
}
