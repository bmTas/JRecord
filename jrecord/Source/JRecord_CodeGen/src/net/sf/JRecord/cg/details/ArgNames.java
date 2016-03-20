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

public interface ArgNames {
	public static final String OPT_SCHEMA = "-Schema";
	public static final String OPT_TEMPLATE = "-Template";
	public static final String OPT_SPLIT = "-split";
	public static final String OPT_GENERATE = "-Generate";
	public static final String OPT_FILE_ORGANISATION = "-FileOrganisation";
	public static final String OPT_LOAD_SCHEMA = "-loadSchema";
	public static final String OPT_PACKAGE = "-package";
	
	public static final String OPT_DROP_COPYBOOK_NAME = "-DropCopybookName";
	public static final String OPT_FONT_NAME = "-font";
	
	public static final String OPT_OUTPUT_DIR = "-outputDirectory";

}
