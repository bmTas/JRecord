/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: RecordEditor's version of JRecord 
 *    
 *    Sub-Project purpose: Low-level IO and record translation  
 *                        code + Cobol Copybook Translation
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 2.1 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.External.base;

import java.util.ArrayList;

import net.sf.JRecord.Common.BasicKeyedField;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.External.Def.BasicConversion;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 * This class holds conversions from external representations of types / formats etc to the internal
 * representations
 *
 *
 * @author Bruce Martin
 *
 */
public final class ExternalConversion {

	public static final int USE_DEFAULT_DB = -1;
	private static AbstractConversion standardConversion ;

	private static String invalidFileChars = null;
	private static String replacementChar = " ";


	private static String[] names = new String [15] ;
	private static int[] keys = new int[15];
	static {
		int i = 0;
		keys[i] = Constants.rtBinaryRecord;		 names[i++] = "BinaryRecord";
		keys[i] = Constants.rtRecordLayout;		 names[i++] = "RecordLayout";
		keys[i] = Constants.rtDelimited;	 	 names[i++] = "Delimited";
		keys[i] = Constants.rtDelimitedAndQuote; names[i++] = "DelimitedAndQuote";
		keys[i] = Constants.RT_XML;				 names[i++] = "XML";
		keys[i] = Constants.rtGroupOfRecords; 	 names[i++] = "GroupOfRecords";
		keys[i] = Constants.rtGroupOfBinaryRecords; names[i++] = "GroupOfBinaryRecords";
		keys[i] = Constants.rtFixedLengthRecords;   names[i++] = "FixedLengthRecords";
		keys[i] = Constants.NULL_INTEGER;		 names[i++] = null;
	}


/*	private static HashMap<String, Integer>[] typeConv = new HashMap[Constants.NUMBER_OF_COPYBOOK_SOURCES];
	private static HashMap<String, Integer>[] formatConv = new HashMap[Constants.NUMBER_OF_COPYBOOK_SOURCES];
	static {
		for (int i = 0; i < typeConv.length; i++) {
			typeConv[i]   = null;
			formatConv[i] = null;
		}
	}*/

	/**
	 * Convert a String (Type Name) to a TypeId
	 * @param dbIdx Database id
	 * @param typeStr Type represented as a char
	 * @return type value
	 */
	public final static int getType(int dbIdx, String typeStr) {
		int type = Constants.NULL_INTEGER;

		try {
			type = Integer.parseInt(typeStr);
		} catch (Exception e) {	}

		if (type < 0) {
			type = getStandardConversion().getType(dbIdx,typeStr);
		}

		if (type < 0) {
			type = Type.ftChar;
		}

		return type;
	}

	/**
	 * Convert a RecordType name to RecordType Id
	 * @param dbIdx DB index (used in recordEditor)
	 * @param typeStr RecordType name
	 * @return Record Type Id
	 */
	public final static int getRecordType(int dbIdx, String typeStr) {
		int type = Constants.NULL_INTEGER;

	   	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (names[i].equalsIgnoreCase(typeStr)) {
    			return keys[i];
    		}
    	}

		try {
			type = Integer.parseInt(typeStr);
		} catch (Exception e) {	}

		return type;
	}


	/**
	 * Get the RecordStyle Id from its name
	 *
	 * @param dbIdx DB index
	 * @param styleStr RecordStyle name
	 * @return RecordStyle
	 */
	public final static int getRecordStyle(int dbIdx, String styleStr) {
		int type = Constants.NULL_INTEGER;

		try {
			type = Integer.parseInt(styleStr);
		} catch (Exception e) {	}

		return type;
	}


	/**
	 * Get File Structure from its name
	 *
	 * @param dbIdx DB index (RecordEditor)
	 * @param fileStructureStr File Structure name
	 *
	 * @return file Structure
	 */
	public final static int getFileStructure(int dbIdx, String fileStructureStr) {
		int fileStructure = Constants.NULL_INTEGER;

		fileStructure = BasicConversion.getStructure(fileStructureStr);

		if (fileStructure == Constants.NULL_INTEGER) {
			try {
				fileStructure = Integer.parseInt(fileStructureStr);
			} catch (Exception e) {	}
		}

		return fileStructure;
	}

	/**
	 * Convert a Type to a Type Name
	 * @param dbIdx DB index
	 * @param type Type Id
	 * @return Type name
	 */
	public final static String getTypeAsString(int dbIdx, int type) {
		String typeStr = Integer.toString(type);

		try {
			typeStr = getStandardConversion().getTypeAsString(dbIdx, type);
		}  catch (Exception e) {
			System.out.println(">> " + dbIdx + ": " + type );
			e.printStackTrace();
		}

		return typeStr;
	}


	/**
	 * Convert a Format Name to a Format Id (Formats are used by the Record Editor)
	 *
	 * @param dbIdx Database id
	 * @param formatStr Format represented as a char
	 * @return Format value
	 */
	public final static int getFormat(int dbIdx, String formatStr) {
		int format = Constants.NULL_INTEGER;

		try {
			format = Integer.parseInt(formatStr);
		} catch (Exception e) {	}

		if (format < 0) {
			format = getStandardConversion().getFormat(dbIdx,formatStr);
		}

		if (format < 0) {
			format = 0;
		}

		return format;
	}


	/**
	 * Convert a Format Id to a Format Name
	 * @param dbIdx Database index
	 * @param format format id
	 * @return Format Name
	 */
	public final static String getFormatAsString(int dbIdx, int format) {
		String formatStr = "";


		if (format > 0) {
			formatStr = getStandardConversion().getFormatAsString(dbIdx,format);
		} else {
			formatStr = "";
		}

		return formatStr;
	}


	/**
	 * Convert File Structure to File Structure name
	 *
	 * @param dbIdx DB index
	 * @param fileStructure file Structure Id
	 * @return FileStructure name
	 */
	public final static String getFileStructureAsString(int dbIdx, int fileStructure) {
		String fileStructureStr = "";

		fileStructureStr = BasicConversion.getStructureName(fileStructure);

		if ("".equals(fileStructureStr) && fileStructure >= 0) {
			try {
				fileStructureStr = Integer.toString(fileStructure);
			} catch (Exception e) {	}
		}

		return fileStructureStr;
	}


	/**
	 * Convert Record Style to its equivalent name
	 * @param dbIdx DB index
	 * @param fileStructure file-Structure Id
	 * @return File-Structure name
	 */
	public final static String getRecordStyleAsString(int dbIdx, int fileStructure) {
		String recordStyleStr = "";

		try {
			recordStyleStr = Integer.toString(fileStructure);
		} catch (Exception e) {	}

		if (fileStructure < 0) {
			recordStyleStr = "";
		}

		return recordStyleStr;
	}

	/**
	 * Convert Record-Type id to Record-Type name
	 *
	 * @param dbIdx database Index
	 * @param recordType Record-Type id
	 *
	 * @return Record-Type Name
	 */
	public final static String getRecordTypeAsString(int dbIdx, int recordType) {
		String recordTypeStr = "";

    	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (keys[i] == recordType) {
    			return names[i];
    		}
    	}

		try {
			recordTypeStr = Integer.toString(recordType);
		} catch (Exception e) {	}

		if (recordType < 0) {
			recordTypeStr = "";
		}

		return recordTypeStr;
	}



	/**
	 * set the standard conversion
	 * @param newStandardConversion the standardConversion to set
	 */
	public final static void setStandardConversion(AbstractConversion newStandardConversion) {
		standardConversion = newStandardConversion;
	}


	/**
	 * Change characters that are invalid in file names to a valid character
	 * @param name filename
	 * @return fixed filename
	 */
    public static final String copybookNameToFileName(String name) {

    	StringBuilder b = new StringBuilder(name);
    	if (invalidFileChars != null) {
    		for (int i = 0; i < invalidFileChars.length(); i++) {
    			Conversion.replace(b, invalidFileChars.substring(i, i+1), replacementChar);
    		}
    	}
		Conversion.replace(b, "\t", " ");
		Conversion.replace(b, "\n", " ");

		return b.toString();
    }

    /**
     * Get all Type Ids / Type Names
     * @return Type Ids / Type Names
     */
    public final static ArrayList<BasicKeyedField> getTypes(int idx) {
		TypeManager manager = TypeManager.getInstance();
    	AbstractConversion conv = getStandardConversion();
    	ArrayList<BasicKeyedField> ret = new ArrayList<BasicKeyedField>(manager.getNumberOfTypes());
    	BasicKeyedField fld;
    	int t;
    	String s;

    	for (int i = 0; i < manager.getNumberOfTypes(); i++) {
    		t = manager.getTypeId(i);
    		s = conv.getTypeAsString(idx, t);
    		if (s != null && ! "".equals(s)) {
    			fld = new BasicKeyedField();
    			fld.key = t;
    			fld.name = s;
    			fld.valid = Boolean.valueOf(conv.isValid(idx, t));
    			ret.add(fld);
    		}
    	}

    	return ret;
    }

    /**
     * Get all Record-Type Id's / Record-Type Names
     * @return Record-Type Id's / Record-Type Names
     */
    public final static ArrayList<BasicKeyedField> getRecordTypes() {
    	ArrayList<BasicKeyedField> ret = new ArrayList<BasicKeyedField>();
    	BasicKeyedField fld;

    	for (int i = 0; i < names.length && ! "".equals(names[i]); i++) {
 			fld = new BasicKeyedField();
			fld.key = keys[i];
			fld.name = names[i];
			ret.add(fld);
    	}

    	return ret;
    }

	/**
	 * Define the invalid File name characters and there replacement Chars
	 *
	 * @param invalidFileCharacters the invalidFileChars to set
	 * @param replacementCharacter Characters to replace the invalid Characters
	 */
	public final static void setInvalidFileChars(String invalidFileCharacters, String replacementCharacter) {
		invalidFileChars = invalidFileCharacters;

		if (replacementChar != null) {
			replacementChar = replacementCharacter;
		}
	}


	public final static String fixDirectory(String directory) {
		String ret = directory;

		if (! ("".equals(ret))) {
			if (ret.endsWith("*")) {
				ret = ret.substring(0, ret.length()-2);
			}
			if (! (ret.endsWith("/") || ret.endsWith("\\"))) {
				ret = ret + Constants.FILE_SEPERATOR;
			}
		}
		return ret;
	}

	public final static String fixFileName(String filename) {
		String ret = filename;

		if (ret != null &&! ("".equals(ret))) {
			StringBuilder b = new StringBuilder(filename);
			Conversion.replace(b, ":", " ");
			Conversion.replace(b, "*", " ");
			ret = b.toString();
		}
		return ret;
	}
	
	/**
	 * Get Dialect Name for a dialect code
	 * @param key dialect-code
	 * @return Dialect name
	 */
	public static String getDialectName(int key) {
		return getStandardConversion().getDialectName(key);
	}

	/**
	 * Get Dialect Code from a dialect name
	 * @param name dialect name
	 * @return Dialect Code
	 */
	public static int getDialect(String name) {
		return getStandardConversion().getDialect(name);
	}


	/**
	 * Get the Conversion class
	 * @return Conversion class
	 */
	private static AbstractConversion getStandardConversion() {

		if (standardConversion == null) {
			standardConversion = new BasicConversion();
		}
		return standardConversion;
	}
}
