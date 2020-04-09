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
      
package net.sf.JRecord.Common;

import java.nio.charset.Charset;

public class CommonBits {
	
	public static final int LT_XML = 1;
	public static final int LT_TEXT = 2;
	public static final int LT_BYTE = 3;
			
	public  static final String LINE_SEPARATOR = System.getProperty("line.separator");
	private static final char[] EMPTY_CHAR_ARRAY = {};
	public  static final String NULL_STRING = new String(EMPTY_CHAR_ARRAY, 0, 0); 
	public  static final Object NULL_VALUE = NULL_STRING;
	private static byte[] EBCDIC_EOL_BYTES = {0x15};
	
	private static int defaultCobolTextFormat = 8; // USE_PROPERTIES_FILE
	private static boolean dropCopybookFromFieldNames = true;
	
	/**
	 * This variable is used in JRecord to control wether CsvLines (based on a List)
	 * is used or the more normal Line / CharLine !!!
	 */
	private static boolean useCsvLine = true;
	
	
	/**
	 * Get the eol chars for a file based on the eol-description and charset
	 * @param defaultEolBytes
	 * @param eolDesc "String" name of eol e.g. "<lf>"
	 * @param charset character-set
	 * @return byte array containing the end-of-line bytes
	 */
	public static byte[] getEolBytes(byte[] defaultEolBytes, String eolDesc, String charset) {
		byte[] recordSep = defaultEolBytes;
		
	    if (Constants.CRLF_STRING.equals(eolDesc)) {
	        recordSep = Constants.CRLF_BYTES;
	    } else if (defaultEolBytes == null || Constants.DEFAULT_STRING.equals(eolDesc) || "".equals(eolDesc)) {
	    	recordSep = Constants.SYSTEM_EOL_BYTES;
	    } else if (Constants.CR_STRING.equals(eolDesc)) {
	        recordSep = Constants.CR_BYTES;
	    } else if (Constants.LF_STRING.equals(eolDesc)) {
	        recordSep = Constants.LF_BYTES;
	    } 
	    if (charset != null && (! "".equals(charset)) && Charset.isSupported(charset)) {
			try {
				byte[] newLineBytes = Conversion.getBytes("\n", charset);
				if (newLineBytes.length == 1 && newLineBytes[0] == EBCDIC_EOL_BYTES[0]) {
		        	recordSep = EBCDIC_EOL_BYTES;
		        } else if (Constants.CRLF_STRING.equals(eolDesc)) {
			        recordSep = Conversion.getBytes("\r\n", charset);
			    } else if (eolDesc == null || Constants.DEFAULT_STRING.equals(eolDesc) || "".equals(eolDesc) ) {
			    	recordSep = Conversion.getBytes(LINE_SEPARATOR, charset);
			    } else if (Constants.CR_STRING.equals(eolDesc)) {
			        recordSep = newLineBytes;
			    } else if (Constants.LF_STRING.equals(eolDesc)) {
			        recordSep = Conversion.getBytes("\r", charset);
			    } else if (defaultEolBytes == null) {
			    	recordSep = newLineBytes;
			    }
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return recordSep;
	}
	
	/**
	 * Get the end-of-line string from the eol-description
	 * @param eolDesc eol-description
	 * @param charset charset being used
	 * @return eol-String
	 */
	public static String getEolString(String eolDesc, String charset) {
		String recordSep = eolDesc;
		
	    if (Constants.CRLF_STRING.equals(eolDesc)) {
	        recordSep = "\r\n";
	    } else if (eolDesc == null || Constants.DEFAULT_STRING.equals(eolDesc) || "".equals(eolDesc)) {
	    	recordSep = LINE_SEPARATOR;
	    } else if (Constants.CR_STRING.equals(eolDesc)) {
	        recordSep = "\n";
	    } else if (Constants.LF_STRING.equals(eolDesc)) {
	        recordSep = "\r";
	    } 
	    if (charset != null && (! "".equals(charset)) && Charset.isSupported(charset)) {
			try {
				byte[] newLineBytes = Conversion.getBytes("\n", charset);
				if (newLineBytes.length == 1 && newLineBytes[0] == EBCDIC_EOL_BYTES[0]) {
		        	recordSep = "\n";
		        } 
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return recordSep;
	}

	public static final boolean useCsvLine() {
		return useCsvLine;
	}

	public static final void setUseCsvLine(boolean useCsvLine) {
		CommonBits.useCsvLine = useCsvLine;
	}

	/**
	 * @return the defaultCobolTextFormat
	 */
	public static final int getDefaultCobolTextFormat() {
		return defaultCobolTextFormat;
	}

	/**
	 * @param defaultCobolTextFormat the defaultCobolTextFormat to set
	 */
	public static final void setDefaultCobolTextFormat(int defaultCobolTextFormat) {
		CommonBits.defaultCobolTextFormat = defaultCobolTextFormat;
	}

	/**
	 * @return the dropCopybookFromFieldNames
	 */
	public static final boolean isDropCopybookFromFieldNames() {
		return dropCopybookFromFieldNames;
	}

	/**
	 * @param dropCopybookFromFieldNames the dropCopybookFromFieldNames to set
	 */
	public static final void setDropCopybookFromFieldNames(
			boolean dropCopybookFromFieldNames) {
		CommonBits.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
	}

	public static boolean areFieldNamesOnTheFirstLine(int fileStructure) {
		boolean ret = false;
		switch (fileStructure) {
		case Constants.IO_CSV_NAME_1ST_LINE:
        case Constants.IO_BIN_NAME_1ST_LINE:
        case Constants.IO_NAME_1ST_LINE:
        case Constants.IO_BIN_CSV_NAME_1ST_LINE:
        case Constants.IO_UNICODE_NAME_1ST_LINE:
        case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
        	 ret = true;
		}
		return ret;
	}
	
	public static boolean isEmbeddedCrSupported(int fileStructure) {
		boolean ret = false;
		switch (fileStructure) {
		case Constants.IO_CSV_NAME_1ST_LINE:
		case Constants.IO_CSV:
        case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
        case Constants.IO_BIN_CSV:
        case Constants.IO_BIN_CSV_NAME_1ST_LINE:
        	 ret = true;
		}
		return ret;
		
	}
	
	
	public static boolean isFontRequired(int fileStructure) {
		boolean ret = false;
		switch (fileStructure) {
		case Constants.IO_FIXED_BYTE_ENTER_FONT:
		case Constants.IO_FIXED_CHAR_ENTER_FONT:
        case Constants.IO_TEXT_BYTE_ENTER_FONT:
        case Constants.IO_TEXT_CHAR_ENTER_FONT:
        	 ret = true;
		}
		return ret;	
	}
	
	public static int translateFileStructureToNotAskFont(int fileStructure) {
		int ret = fileStructure;
		switch (fileStructure) {
		case Constants.IO_FIXED_BYTE_ENTER_FONT:	ret = Constants.IO_FIXED_LENGTH;		break;
		case Constants.IO_FIXED_CHAR_ENTER_FONT:	ret = Constants.IO_FIXED_LENGTH_CHAR;	break;
        case Constants.IO_TEXT_BYTE_ENTER_FONT:		ret = Constants.IO_BIN_TEXT;			break;
        case Constants.IO_TEXT_CHAR_ENTER_FONT:		ret = Constants.IO_UNICODE_TEXT;		break;
		}
		return ret;	
	}

	
	public static int getLineType(int fileStructure) {
    	switch (fileStructure) {
    	case Constants.IO_XML_BUILD_LAYOUT:
    	case Constants.IO_XML_USE_LAYOUT:   		
       		return LT_XML;
    	case Constants.IO_FIXED_LENGTH_CHAR:
    	case Constants.IO_FIXED_CHAR_ENTER_FONT:
    	case Constants.IO_TEXT_CHAR_ENTER_FONT:
    	case Constants.IO_UNICODE_CSV:
    	case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
    	case Constants.IO_UNICODE_NAME_1ST_LINE:
    	case Constants.IO_UNICODE_TEXT:
    		return LT_TEXT;
    	default:
    		return LT_BYTE;
    	}
	}
	
	public static final boolean checkFor(byte[] buffer, int pos, byte[] search) {
		//System.out.println("!! " + pos + " " + (search.length - 1) );
		if (search == null || pos < search.length - 1 || pos >= buffer.length || search.length == 0) {
			return false;
		}

		int bufferStart = pos - search.length + 1;
		for (int i = 0; i < search.length; i++) {
			if (search[i] != buffer[bufferStart + i]) {
				return false;
			}
		}

		return true;
	}
}
