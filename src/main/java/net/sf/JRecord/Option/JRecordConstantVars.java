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

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;


/**
 * Holds JRecord Constants for use in scripting languages  
 * where it is challenging to access variables in interfaces like JavaScript (jjs)
 * 
 * @author Bruce Martin
 *
 */
public class JRecordConstantVars {

    private JRecordConstantVars() {
        super();
    }
	
	/* ----------------------------------------------------------------------
	 * 
	 * Cobol Dialects:
	 */
    public static int FMT_MAINFRAME		  = ICopybookDialects.FMT_MAINFRAME;
    public static int FMT_FUJITSU		  = ICopybookDialects.FMT_FUJITSU;
    public static int FMT_BIG_ENDIAN		  = ICopybookDialects.FMT_BIG_ENDIAN;
    public static int FMT_FS2000			  = ICopybookDialects.FMT_FS2000;

    public static int FMT_FS2000_BE		  = ICopybookDialects.FMT_FS2000_BE;
    public static int FMT_MAINFRAME_COMMA_DECIMAL = ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL;
    public static int FMT_FUJITSU_COMMA_DECIMAL = ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL;

    public static int FMT_GNU_COBOL        = ICopybookDialects.FMT_OPEN_COBOL;
    public static int FMT_GNU_COBOL_MVS    = ICopybookDialects.FMT_GNU_COBOL_MVS;
    public static int FMT_GNU_COBOL_MF     = ICopybookDialects.FMT_GNU_COBOL_MF;

    public static int FMT_GNU_COBOL_BE     = ICopybookDialects.FMT_GNU_COBOL_BE;
    public static int FMT_GNU_COBOL_BE_MVS = ICopybookDialects.FMT_OPEN_COBOL_MVS_BE;
    public static int FMT_GNU_COBOL_BE_MF  = ICopybookDialects.FMT_OC_MICRO_FOCUS_BE;
    
	/* -------------------------------------------------------------------------
	 * 
	 * Cobol Copybook split options
	 */
    public static int SPLIT_NONE              = ICobolSplitOptions.SPLIT_NONE;
    public static int SPLIT_REDEFINE          = ICobolSplitOptions.SPLIT_REDEFINE;
    public static int SPLIT_01_LEVEL			 = ICobolSplitOptions.SPLIT_01_LEVEL;
    public static int SPLIT_HIGHEST_REPEATING = ICobolSplitOptions.SPLIT_HIGHEST_REPEATING;

    /* ------------------------------------------------------------------------
     * 
     * IO Options
     */
    public static int IO_STANDARD_TEXT_FILE	 = IFileStructureConstants.IO_STANDARD_TEXT_FILE;

    public static int IO_FIXED_LENGTH_RECORDS = IFileStructureConstants.IO_FIXED_LENGTH_RECORDS;
    public static int IO_BINARY_IBM_4680		 = IFileStructureConstants.IO_BINARY_IBM_4680;
    public static int IO_VB					 = IFileStructureConstants.IO_VB;
    public static int IO_VB_DUMP				 = IFileStructureConstants.IO_VB_DUMP;
    public static int IO_VB_DUMP2			 = IFileStructureConstants.IO_VB_DUMP2;
    public static int IO_VB_FUJITSU			 = IFileStructureConstants.IO_VB_FUJITSU;
    public static int IO_VB_GNU_COBOL		 = IFileStructureConstants.IO_VB_GNU_COBOL;
    public static int IO_BIN_TEXT			 = IFileStructureConstants.IO_BIN_TEXT;
    public static int IO_FIXED_LENGTH_CHAR	 = IFileStructureConstants.IO_FIXED_LENGTH_CHAR		        ;
    @SuppressWarnings("deprecation")
    public static int IO_VBS					 = IFileStructureConstants.IO_VBS;


    public static int IO_CONTINOUS_NO_LINE_MARKER = IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER;

    public static int IO_CSV				     = IFileStructureConstants.IO_CSV;
    public static int IO_BIN_CSV				 = IFileStructureConstants.IO_BIN_CSV;
    public static int IO_UNICODE_CSV			 = IFileStructureConstants.IO_UNICODE_CSV;

    public static int IO_CSV_NAME_1ST_LINE	 = IFileStructureConstants.IO_CSV_NAME_1ST_LINE		        ;
    public static int IO_BIN_CSV_NAME_1ST_LINE= IFileStructureConstants.IO_BIN_CSV_NAME_1ST_LINE;
    public static int IO_UNICODE_CSV_NAME_1ST_LINE = IFileStructureConstants.IO_UNICODE_CSV_NAME_1ST_LINE	        ;

    public static int IO_NAME_1ST_LINE		 = IFileStructureConstants.IO_NAME_1ST_LINE;
    public static int IO_BIN_NAME_1ST_LINE	 = IFileStructureConstants.IO_BIN_NAME_1ST_LINE		        ;
    public static int IO_UNICODE_NAME_1ST_LINE= IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE;

    public static int IO_STANDARD_UNICODE_TEXT_FIL = IFileStructureConstants.IO_STANDARD_UNICODE_TEXT_FILE        ;

    public static int IO_UNICODE_TEXT	= IFileStructureConstants.IO_UNICODE_TEXT;
    public static int IO_FIXED_LENGTH	= IFileStructureConstants.IO_FIXED_LENGTH;
    public static int IO_TEXT_LINE		= IFileStructureConstants.IO_TEXT_LINE;
    
    
    /* ------------------------------------------------------------------------------
     * 
     * Option to reformat Cobol names when converting to other languages
     * 
     */
    public static int RO_LEAVE_ASIS = IReformatFieldNames.RO_LEAVE_ASIS;
    public static int RO_MINUS_TO_UNDERSCORE = IReformatFieldNames.RO_UNDERSCORE;
    public static int RO_CAMEL_CASE = IReformatFieldNames.RO_CAMEL_CASE;


}
