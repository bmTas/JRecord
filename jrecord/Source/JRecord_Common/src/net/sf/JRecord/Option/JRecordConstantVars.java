/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
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
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.Option;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;


/**
 * Holds JRecord Constants for use in scripting languages  
 * where it is difficult to access variables in interfaces like
 * like javascript (jjs)
 * 
 * @author Bruce Martin
 *
 */
public class JRecordConstantVars {
	
	public static final JRecordConstantVars INSTANCE = new JRecordConstantVars();
	
	
	/* ----------------------------------------------------------------------
	 * 
	 * Cobol Dialects:
	 */
    public final int FMT_MAINFRAME		  = ICopybookDialects.FMT_MAINFRAME; 
    public final int FMT_FUJITSU		  = ICopybookDialects.FMT_FUJITSU; 
    public final int FMT_BIG_ENDIAN		  = ICopybookDialects.FMT_BIG_ENDIAN; 
    public final int FMT_FS2000			  = ICopybookDialects.FMT_FS2000; 
                                         
    public final int FMT_FS2000_BE		  = ICopybookDialects.FMT_FS2000_BE; 
//    public final int FMT_MICRO_FOCUS			 = ICopybookDialects.FMT_MICRO_FOCUS;                                      
    public final int FMT_MAINFRAME_COMMA_DECIMAL = ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL;
    public final int FMT_FUJITSU_COMMA_DECIMAL	 = ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL;
       
	public final int FMT_GNU_COBOL        = ICopybookDialects.FMT_OPEN_COBOL;
	public final int FMT_GNU_COBOL_MVS    = ICopybookDialects.FMT_GNU_COBOL_MVS;
	public final int FMT_GNU_COBOL_MF     = ICopybookDialects.FMT_GNU_COBOL_MF;

	public final int FMT_GNU_COBOL_BE     = ICopybookDialects.FMT_GNU_COBOL_BE;
	public final int FMT_GNU_COBOL_BE_MVS = ICopybookDialects.FMT_OPEN_COBOL_MVS_BE;
	public final int FMT_GNU_COBOL_BE_MF  = ICopybookDialects.FMT_OC_MICRO_FOCUS_BE;
    
	/* -------------------------------------------------------------------------
	 * 
	 * Cobol Copybook split options
	 */
    public final int SPLIT_NONE              = ICobolSplitOptions.SPLIT_NONE;
    public final int SPLIT_REDEFINE          = ICobolSplitOptions.SPLIT_REDEFINE;
    public final int SPLIT_01_LEVEL			 = ICobolSplitOptions.SPLIT_01_LEVEL;
    public final int SPLIT_HIGHEST_REPEATING = ICobolSplitOptions.SPLIT_HIGHEST_REPEATING; 

    /* ------------------------------------------------------------------------
     * 
     * IO Options
     */
    public final int IO_STANDARD_TEXT_FILE	 = Constants.IO_STANDARD_TEXT_FILE;
		                                           
    public final int IO_FIXED_LENGTH_RECORDS = Constants.IO_FIXED_LENGTH_RECORDS;
    public final int IO_BINARY_IBM_4680		 = Constants.IO_BINARY_IBM_4680;
    public final int IO_VB					 = Constants.IO_VB;
    public final int IO_VB_DUMP				 = Constants.IO_VB_DUMP;
    public final int IO_VB_FUJITSU			 = Constants.IO_VB_FUJITSU;
    public final int IO_VB_GNU_COBOL		 = Constants.IO_VB_GNU_COBOL;                                                                               
    public final int IO_BIN_TEXT			 = Constants.IO_BIN_TEXT;
    public final int IO_FIXED_LENGTH_CHAR	 = Constants.IO_FIXED_LENGTH_CHAR		        ;
    @SuppressWarnings("deprecation")
	public final int IO_VBS					 = Constants.IO_VBS;
                                                                                               
                                                                                                
    public final int IO_CONTINOUS_NO_LINE_MARKER = Constants.IO_CONTINOUS_NO_LINE_MARKER;
                                                                                                 
    public final int IO_CSV				     = Constants.IO_CSV;
    public final int IO_BIN_CSV				 = Constants.IO_BIN_CSV;
    public final int IO_UNICODE_CSV			 = Constants.IO_UNICODE_CSV;
                                                                                        
    public final int IO_CSV_NAME_1ST_LINE	 = Constants.IO_CSV_NAME_1ST_LINE		        ;
    public final int IO_BIN_CSV_NAME_1ST_LINE= Constants.IO_BIN_CSV_NAME_1ST_LINE;
    public final int IO_UNICODE_CSV_NAME_1ST_LINE = Constants.IO_UNICODE_CSV_NAME_1ST_LINE	        ;
                                                                                        
    public final int IO_NAME_1ST_LINE		 = Constants.IO_NAME_1ST_LINE;        
    public final int IO_BIN_NAME_1ST_LINE	 = Constants.IO_BIN_NAME_1ST_LINE		        ;
    public final int IO_UNICODE_NAME_1ST_LINE= Constants.IO_UNICODE_NAME_1ST_LINE;
                                                                                        
    public final int IO_STANDARD_UNICODE_TEXT_FIL = Constants.IO_STANDARD_UNICODE_TEXT_FILE        ;
    
    public final int IO_UNICODE_TEXT	= Constants.IO_UNICODE_TEXT;
    public final int IO_FIXED_LENGTH	= Constants.IO_FIXED_LENGTH;	                                      
    public final int IO_TEXT_LINE		= Constants.IO_TEXT_LINE;	
    
    
    /* ------------------------------------------------------------------------------
     * 
     * Option to reformat Cobol names when converting to other languages
     * 
     */
    public final int RO_LEAVE_ASIS = IReformatFieldNames.RO_LEAVE_ASIS;
    public final int RO_MINUS_TO_UNDERSCORE = IReformatFieldNames.RO_UNDERSCORE; 
	public final int RO_CAMEL_CASE = IReformatFieldNames.RO_CAMEL_CASE;


}
