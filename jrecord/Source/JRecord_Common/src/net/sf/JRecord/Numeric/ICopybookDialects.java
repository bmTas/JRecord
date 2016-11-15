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

	public static final int FMT_INTEL = 0;
	public static final int FMT_MAINFRAME = 1;
	public static final int FMT_FUJITSU = 2;
	public static final int FMT_BIG_ENDIAN = 3;
	public static final int FMT_GNU_COBOL = 4;
	public static final int FMT_FS2000 = 5;
	public static final int FMT_GNU_COBOL_MVS = 6;
	public static final int FMT_GNU_COBOL_MF = 7;
	public static final int FMT_GNU_COBOL_BE = 8;
	public static final int FMT_FS2000_BE = 9;
	public static final int FMT_GNU_COBOL_MVS_BE = 10;
	public static final int FMT_OC_MICRO_FOCUS_BE = 11;
	public static final int FMT_MICRO_FOCUS = 21;
	public static final int FMT_MAINFRAME_COMMA_DECIMAL = 31;
	public static final int FMT_FUJITSU_COMMA_DECIMAL = 32;
	

	public static final int FMT_OPEN_COBOL = FMT_GNU_COBOL;
	public static final int FMT_OPEN_COBOL_MVS = FMT_GNU_COBOL_MVS;
	public static final int FMT_OC_MICRO_FOCUS = FMT_GNU_COBOL_MF;
	public static final int FMT_OPEN_COBOL_BE = FMT_GNU_COBOL_BE;
	public static final int FMT_OPEN_COBOL_MVS_BE = FMT_GNU_COBOL_MVS_BE;


}