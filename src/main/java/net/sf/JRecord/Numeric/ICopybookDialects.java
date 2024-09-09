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
      
package net.sf.JRecord.Numeric;

public interface ICopybookDialects {

	int FMT_INTEL = 0;
	int FMT_MAINFRAME = 1;
	int FMT_FUJITSU = 2;
	int FMT_BIG_ENDIAN = 3;
	int FMT_GNU_COBOL = 4;
	int FMT_FS2000 = 5;
	int FMT_GNU_COBOL_MVS = 6;
	int FMT_GNU_COBOL_MF = 7;
	int FMT_GNU_COBOL_BE = 8;
	int FMT_FS2000_BE = 9;
	int FMT_GNU_COBOL_MVS_BE = 10;
	int FMT_OC_MICRO_FOCUS_BE = 11;
	int FMT_MICRO_FOCUS = 21;
	int FMT_MAINFRAME_COMMA_DECIMAL = 31;
	int FMT_FUJITSU_COMMA_DECIMAL = 32;


	int FMT_OPEN_COBOL = FMT_GNU_COBOL;
	int FMT_OC_MICRO_FOCUS = FMT_GNU_COBOL_MF;
	int FMT_OPEN_COBOL_MVS_BE = FMT_GNU_COBOL_MVS_BE;

	String FMT_INTEL_NAME = "Intel";
	String FMT_MAINFRAME_NAME = "Mainframe";
	String FMT_FUJITSU_NAME = "Fujitsu";
	String FMT_BIG_ENDIAN_NAME = "Big-Endian (Old)";
	String FMT_GNU_COBOL_NAME = "GNU Cobol Little Endian (Intel)";
	String FMT_FS2000_NAME = "GNU Cobol bs2000 Little Endian (Intel)";
	String FMT_GNU_COBOL_MVS_NAME = "GNU Cobol MVS Little Endian (Intel)";
	String FMT_GNU_COBOL_MF_NAME = "GNU Cobol Micro Focus (Intel)";
	String FMT_GNU_COBOL_BE_NAME = "GNU Cobol Big Endian";
	String FMT_FS2000_BE_NAME = "GNU Cobol bs2000 Big Endian";
	String FMT_GNU_COBOL_MVS_BE_NAME = "GNU Cobol MVS Big Endian";
	String FMT_OC_MICRO_FOCUS_BE_NAME = "GNU Cobol Micro Focus Big E";
	String FMT_MAINFRAME_COMMA_DECIMAL_NAME = "Mainframe Decimal_Point=','";
	String FMT_FUJITSU_COMMA_DECIMAL_NAME = "Fujitsu Decimal_Point=','";}