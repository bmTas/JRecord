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
      
package net.sf.JRecord.External.Def;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.constantNames.ConstantDetails;
import net.sf.JRecord.constantNames.ConstantNames;


public class BasicConversion implements AbstractConversion {
	
	private static ConstantNames.ConstantConversions constantConversions = ConstantNames.getConstantConversions();

//	private static final int numberOfEntries;
//	private static String[] names = new String [40] ;
//    private static String[] externalNames = new String [40] ;
//    private static int[] keys = new int[40];
//    private static int[] keysIdx = new int[200];

//    static {
//       int i = 0; 
    	
// 	   String rdDefault = "Default";ƒ
// 	   String rdFixed = "Fixed Length Binary";
// 	   String rdLineBin = "Line based Binary";
// 	   String rdVb = "Mainframe VB (rdw based) Binary";
// 	   String rdVbDump = "Mainframe VB Dump: includes Block length";
////   	   String rdOcVb = "Open Cobol VB";
//
// 	   
//    	
//        keys[i] = IFileStructureConstants.IO_DEFAULT;                externalNames[i] = "Default";                 names[i++] = rdDefault;
//        keys[i] = IFileStructureConstants.IO_TEXT_LINE;              externalNames[i] = "Text";                    names[i++] = "Text IO";
//        keys[i] = IFileStructureConstants.IO_BIN_TEXT;               externalNames[i] = "Byte_Text";               names[i++] = "Text IO (byte Based)";
//        keys[i] = IFileStructureConstants.IO_UNICODE_TEXT;           externalNames[i] = "Text_Unicode";            names[i++] = "Text IO (Unicode)";
//        keys[i] = IFileStructureConstants.IO_FIXED_LENGTH;           externalNames[i] = "Fixed_Length";            names[i++] = rdFixed;
//        keys[i] = IFileStructureConstants.IO_FIXED_LENGTH_CHAR;      externalNames[i] = "Fixed_Length_Char";       names[i++] = "Fixed Length Char";
//        
//        keys[i] = IFileStructureConstants.IO_BINARY_IBM_4680;        externalNames[i] = "Binary";                  names[i++] = rdLineBin;
//        keys[i] = IFileStructureConstants.IO_VB;                     externalNames[i] = "Mainframe_VB";            names[i++] = rdVb;
//        keys[i] = IFileStructureConstants.IO_VB_DUMP;                externalNames[i] = "Mainframe_VB_As_RECFMU";  names[i++] = rdVbDump;
//        keys[i] = IFileStructureConstants.IO_VB_DUMP2;               externalNames[i] = "VB_DUMP2"; 				 names[i++] = "VB_DUMP2";
//        keys[i] = IFileStructureConstants.IO_VB_FUJITSU;             externalNames[i] = "FUJITSU_VB";              names[i++] = "Fujitsu Variable Binary";
//        keys[i] = IFileStructureConstants.IO_VB_GNU_COBOL;           externalNames[i] = "Gnu_Cobol_VB";            names[i++] = "GNU Cobol VB";
//        keys[i] = IFileStructureConstants.IO_MICROFOCUS;             externalNames[i] = "Microfocus_Format";       names[i++] = "Experimental Microfocus Header File";
//        keys[i] = IFileStructureConstants.IO_UNKOWN_FORMAT;          externalNames[i] = "UNKOWN_FORMAT";           names[i++] = "Unknown File Format";
//        keys[i] = IFileStructureConstants.IO_WIZARD;                 externalNames[i] = "FILE_WIZARD";             names[i++] = "File Wizard";
//        keys[i] = IFileStructureConstants.IO_CSV;                    externalNames[i] = "CSV_EMBEDDED_CR";         names[i++] = "Csv Embedded Cr";
//        keys[i] = IFileStructureConstants.IO_UNICODE_CSV;            externalNames[i] = "UNICODE_CSV_EMBEDDED_CR"; names[i++] = "Unicode Csv Embedded Cr";
//        keys[i] = IFileStructureConstants.IO_NAME_1ST_LINE;          externalNames[i] = "CSV_NAME_1ST_LINE";       names[i++] = "Csv Name on 1st line";
//        keys[i] = IFileStructureConstants.IO_CSV_NAME_1ST_LINE;      externalNames[i] = "CSV_NAME_1ST_LINE_EMBEDDED_CR"; names[i++] = "Csv Name on 1st line (Embedded Cr)";
//        keys[i] = IFileStructureConstants.IO_BIN_NAME_1ST_LINE;      externalNames[i] = "Byte_Text_NAME_1ST_LINE"; names[i++] = "Text IO (byte Based) name 1st Line";
//        keys[i] = IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE;  externalNames[i] = "UNICODE_CSV_NAME_1ST_LINE";  names[i++] = "Unicode Name on 1st line";
//        keys[i] = IFileStructureConstants.IO_UNICODE_CSV_NAME_1ST_LINE;externalNames[i] = "UNICODE_CSV_NAME_1ST_LINE_EMBEDDED_CR";      names[i++] = "Unicode Name on 1st line (Embedded Cr)";
// 
//        keys[i] = IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE;  externalNames[i] = "UNICODE_CSV_NAME_1ST_LINE_"; names[i++] = "Unicode Name on 1st line";
//        keys[i] = IFileStructureConstants.IO_GENERIC_CSV;            externalNames[i] = "CSV_GENERIC";            names[i++] = "Generic CSV (Choose details at run time)";
//
//        keys[i] = IFileStructureConstants.IO_XML_USE_LAYOUT;         externalNames[i] = "XML_Use_Layout";         names[i++] = "XML - Existing Layout";
//        keys[i] = IFileStructureConstants.IO_XML_BUILD_LAYOUT;       externalNames[i] = "XML_Build_Layout";       names[i++] = "XML - Build Layout";
//        keys[i] = IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER;       externalNames[i] = "Continuous";  		    names[i++] = "Continuous no eol marker";
//        keys[i] = IFileStructureConstants.IO_VBS;                    externalNames[i] = "Mainframe_VBS";            names[i++] = "Variable Block Spanned (VBS)";
////        keys[i] = IFileStructureConstants.IO_VB_GNU_COBOL;           externalNames[i] = "Open_Cobol_VB";           names[i++] = "Open Cobol VB";;
//        keys[i] = Constants.NULL_INTEGER;              externalNames[i] = null;                     names[i] = null;
//
//        keys[i] = IFileStructureConstants.IO_FIXED_BYTE_ENTER_FONT;  externalNames[i] = "FIXED_BYTE_ENTER_FONT";   names[i++] = "Fixed Byte, enter font";
//        keys[i] = IFileStructureConstants.IO_FIXED_CHAR_ENTER_FONT;  externalNames[i] = "FIXED_CHAR_ENTER_FONT";   names[i++] = "Fixed Char, enter font";
//        keys[i] = IFileStructureConstants.IO_TEXT_BYTE_ENTER_FONT;   externalNames[i] = "TEXT_BYTE_ENTER_FONT";    names[i++] = "Text IO (Byte), Enter Font";
//        keys[i] = IFileStructureConstants.IO_TEXT_CHAR_ENTER_FONT;   externalNames[i] = "TEXT_CHAR_ENTER_FONT";    names[i++] = "Text IO (Char), Enter Font";
//        
//        if (i < keys.length) {
//        	keys[i] = Constants.NULL_INTEGER;
//        }
//
//        
//        
//        numberOfEntries = i;
//        
//        Arrays.fill(keysIdx, -1);
//        
//        for (int j = 0; j < numberOfEntries; j++) {
//        	int idx = keys[j];
//        	if (idx >= 0 && keysIdx[idx] < 0) {
//        		keysIdx[idx] = j;
// //       		System.out.println("---->>> " + idx + ", " + j + "\t" + externalNames[j] + "\t!\t" + names[j]);
//        	}
//        }
//    }
    
//	private String[] typeNames ;
//	private HashMap<String, Integer> typeNumbers;
	
//	private HashMap<String, Integer> dialectLookup = new HashMap<String, Integer>(50);
//	private HashMap<Integer, String> dialectNameLookup = new HashMap<Integer, String>(50);

//	/**
//	 * Basic Type / Format conversion (for use in JRecord; RecordEditor has
//	 * its own (database based conversion).
//	 */
//	public BasicConversion() {
//		TypeManager manager = TypeManager.getInstance();
//		typeNames = new String[manager.getNumberOfTypes()];
//		typeNumbers = new HashMap<String, Integer>(manager.getNumberOfTypes() * 2);
//
//		for (int i=0; i < typeNames.length; i++) {
//			typeNames[i] = "";
//		}
//
//		setName(Type.ftChar  , "Char");
//		setName(Type.ftNumAnyDecimal  , "Number any decimal", "NumAnyDecimal");
//		setName(Type.ftNumOrEmpty  , "Number any decimal or Empty", "NumberOrEmpty");
//		setName(Type.ftPositiveNumAnyDecimal  , "PositiveNumAnyDecimal", "Number (+ve) any decimal");
//		setName(Type.ftCharRightJust      , "Char (right justified)");
//		setName(Type.ftCharNullTerminated , "Char Null terminated");
//		setName(Type.ftCharNullPadded     , "Char Null padded");
//		setName(Type.ftCharNoTrim         , "Char (no Trim)");
//		setName(Type.ftHex                , "Hex Field");
//		setName(Type.ftNumLeftJustified   , "Num (Left Justified)");
//		setName(Type.ftNumRightJustified  , "Num (Right Justified space padded)");
//		setName(Type.ftNumRightJustifiedPN, "Num (Right Justified space padded) +/- sign");
//		setName(Type.ftNumRightJustCommaDp, "Num (Right Just space padded, \",\" Decimal)");
//		setName(Type.ftNumRightJustCommaDpPN, "Num (Right Just space padded, \",\" Decimal) +/- sign", "Num (Right Just space padded, \",\" Decimal) +/- sig");
//		setName(Type.ftNumZeroPadded      , "Num (Right Justified zero padded)", "Zero Padded Number with sign=+/-");
//		setName(Type.ftNumZeroPaddedPN    , "Num (Right Justified zero padded +/- sign)", "Zero Padded Number with sign=+/-");
//		setName(Type.ftAssumedDecimal         , "Num Assumed Decimal (Zero padded)");
//		setName(Type.ftAssumedDecimalPositive , "Num Assumed Decimal (+ve)");
//		setName(Type.ftNumZeroPaddedPositive  , "Num (Right Justified zero padded positive)", "Positive Zero Padded Number");
//		setName(Type.ftNumCommaDecimal        , "Zero Padded Number decimal=\",\"");
//		setName(Type.ftNumCommaDecimalPN      , "Zero Padded Number decimal=\",\" sign=+/-");
//		setName(Type.ftNumCommaDecimalPositive, "Zero Padded Number decimal=\",\" (only +ve)");
//
//
//		setName(Type.ftSignSeparateLead   , "Num Sign Separate Leading");
//		setName(Type.ftSignSeparateTrail  , "Num Sign Separate Trailing");
//		setName(Type.ftSignSepLeadActualDecimal   , "Num Sign Sep Leading Actual Dec");
//		setName(Type.ftSignSepTrailActualDecimal  , "Num Sign Sep Trailing Actual Dec");
//		setName(Type.ftDecimal            , "Decimal");
//		setName(Type.ftBinaryInt          , "Binary Integer");
//		setName(Type.ftBinaryIntPositive  , "Binary Integer (only +ve)");
//		setName(Type.ftPostiveBinaryInt   , "Positive Binary Integer");
//		typeNumbers.put("Postive Binary Integer".toLowerCase(), Integer.valueOf(Type.ftPostiveBinaryInt ));
//		setName(Type.ftFloat              , "Float");
//		setName(Type.ftDouble             , "Double");
//		setName(Type.ftBit  , "Bit");
//		setName(Type.ftPackedDecimal         , "Mainframe Packed Decimal (comp-3)");
//		setName(Type.ftPackedDecimalPostive  , "Mainframe Packed Decimal (+ve)");
//		setName(Type.ftZonedNumeric  , "Mainframe Zoned Numeric");
//		typeNumbers.put("Binary Integer Big Edian (Mainframe, AIX etc)".toLowerCase(), Integer.valueOf(Type.ftBinaryBigEndian ));
////		setName(Type.ftZonedLeading  , "Mainframe Zoned Leading");
//		setName(Type.ftBinaryBigEndian  , "Binary Integer Big Endian (Mainframe?)", "Binary Integer Big Endian (Mainframe, AIX etc)");
//		setName(Type.ftBinaryBigEndianPositive  , "Binary Integer Big Endian (only +ve)", "Binary Integer Big Endian (only +ve )");
//		setName(Type.ftPositiveBinaryBigEndian  , "Positive Integer Big Endian", "Positive Integer (Big Endian)");
//		setName(Type.ftFjZonedNumeric  , "Fujitsu Zoned Numeric");
//		setName(Type.ftGnuCblZonedNumeric  , "GNU Cobol Zoned Numeric");
//
//
//		setName(Type.ftRmComp, "Rm Cobol Comp");
//		setName(Type.ftRmCompPositive  , "Rm Cobol Comp (+ve)", "RM Cobol Positive Comp");
//		setName(Type.ftCheckBoxBoolean , "Check Box (Boolean)");
//
//
//		setName(Type.ftDate  , "Date - Format in Parameter field");
//		setName(Type.ftDateYMD  , "Date - YYMMDD");
//		setName(Type.ftDateYYMD  , "Date - YYYYMMDD");
//		setName(Type.ftDateDMY  , "Date - DDMMYY");
//		setName(Type.ftDateDMYY  , "Date - DDMMYYYY");
//		setName(Type.ftCheckBoxTrue  , "Check Box True / Space");
//		setName(Type.ftCheckBoxY  , "Checkbox Y/<null>", "CheckBox Y/null");
//		setName(Type.ftCheckBoxYN  , "Checkbox Y/N");
//		setName(Type.ftCheckBoxTF  , "Checkbox T/F");
//		setName(Type.ftCsvArray  , "CSV array");
//		setName(Type.ftXmlNameTag  , "XML Name Tag");
//		setName(Type.ftMultiLineEdit  , "Edit Multi Line field");
//
//		setName(Type.ftCharRestOfFixedRecord  , "Char Rest of Fixed Length");
//		setName(Type.ftCharRestOfRecord  , "Char Rest of Record");
//		setName(Type.ftMultiLineChar  , "Char (Multi-Line)", "Char Multi Line");
//		
//		
//		setName(Type.ftCharMultiLine  , "Char (Multi-Line) - Old");
//		setName(Type.ftHtmlField  , "Html Field");
//		setName(Type.ftArrayField, "Array Field");
//		setName(Type.ftRecordEditorType, "RecordEditor_Type");
		
//		ConversionManager dialectMgr = ConversionManager.getInstance();
//		
//		for (int i = 0; i < dialectMgr.getNumberOfEntries(); i++) {
//			Convert converter = dialectMgr.getConverter(i);
//			String name = Conversion.replace(converter.getName(), " ", "_").toString();
//			dialectLookup.put(name.toLowerCase(), converter.getIdentifier());
//			dialectLookup.put(converter.getName().toLowerCase(), converter.getIdentifier());
//			dialectLookup.put(Integer.toString(converter.getIdentifier()), converter.getIdentifier());
////			System.out.println(converter.getIdentifier() + " " + name + " " + converter.getName());
//			dialectNameLookup.put(converter.getIdentifier(), name);
//		}
//	}

//	/**
//	 * Set The type name
//	 * @param type type Id
//	 * @param name Type Name
//	 */
//	private Integer setName(int type, String name) {
//		typeNames[TypeManager.getInstance().getIndex(type)] = name;
//		Integer typeId = Integer.valueOf(type);
//		typeNumbers.put(name.toLowerCase(), typeId);
//		if (name.length() > 40) {
//			typeNumbers.put(name.toLowerCase().substring(0, 40), typeId);
//		}
//		return typeId;
//	}
//
//	/**
//	 * Set The type name
//	 * @param type type Id
//	 * @param name Type Name
//	 */
//	private void setName(int type, String name, String altname) {
//		Integer typeId = setName(type, name);
//		typeNumbers.put(altname.toLowerCase(), typeId);
//		if (altname.length() > 40) {
//			typeNumbers.put(altname.toLowerCase().substring(0, 40), typeId);
//		}
//	}


	@Override
	public int getFormat(int idx, String format) {
		if (format != null && ! "".equals(format)) {
			try {
				return Integer.parseInt(format);
			} catch (Exception e) {
			}
		}
		return 0;
	}

	@Override
	public String getFormatAsString(int idx, int format) {
		return Integer.toString(format);
	}

	@Override
	public int getType(int idx, String type) {
		ConstantDetails typeDetails = constantConversions.getTypeNames().getConstantDetails(type);
		return typeDetails == null ? Type.ftChar : typeDetails.getCode();
	}
	

	@Override
	public String getTypeAsString(int idx, int type) {
		return constantConversions.getTypeNames().getConstantName(type);
	}

	
	public boolean isValidTypeName(String name) {

		return name != null && name.length() > 0 
				&& constantConversions.getTypeNames().getConstantDetails(name) != null;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.Def.AbstractConversion#isValid(int, int)
	 */
	@Override
	public boolean isValid(int idx, int type) {
		//String s = typeNames[TypeManager.getInstance().getIndex(type)];
		return constantConversions.getTypeNames().getConstantDetails(type) != null;
	}

	
	@Override
	public int getDialect(String name) {
		return constantConversions.getDialects().getCode(name);
	}
	
	
	@Override
	public String getDialectName(int key) {
		String ret = "";
				
//		String lookup = dialectNameLookup.get(key);
//		if (lookup != null) {
//			ret = lookup;
		ConstantDetails dialect = constantConversions.getDialects().getConstantDetails(key);
		if (dialect != null) {
			ret = dialect.getExternalName();
		}
		
		return ret;
	}

    /* (non-Javadoc)
	 * @see net.sf.JRecord.IO.AbstractLineIOProvider#getStructureName(int)
	 */
    public static String getStructureName(int fileStructure) {
    	ConstantDetails constantDetails = constantConversions.getFileStructureNames().getConstantDetails(fileStructure);
    	return constantDetails == null ? "" : constantDetails.getExternalName();
    	
////    	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
////    		if (keys[i] == fileStructure) {
////    			return externalNames[i];
////    		}
////    	}
//    	if (fileStructure >= 0 && fileStructure < keysIdx.length ) {
//    		int idx = keysIdx[fileStructure];
//    		if (idx >= 0) {
////    			System.out.println("===> " + fileStructure + ", " + idx + " " + externalNames[idx]);
//    			return externalNames[idx];
//    		}
//    	}
//    	return "";
    }

    public static String getOldStructureName(int fileStructure) {
    	ConstantDetails constantDetails = constantConversions.getFileStructureNames().getConstantDetails(fileStructure);
    	return constantDetails == null ? "" : constantDetails.getExternalName();
    }

	public static String getStructureNameForIndex(int index) {
		return getStructureFromIndex(index).getExternalName();
		//return externalNames[index];
	}

	private static ConstantDetails getStructureFromIndex(int index) {
		return constantConversions.getFileStructureNames().getConstantList().get(index);
	}


	/**
     * Convert a structure-name to a file-Structure identifier
     * @param name Name of the File Structure
     * @return The file Structure
     */
    public static int getStructure(String name) {
    	return constantConversions.getFileStructureNames().getCode(name);
    }



    public static int getFileStructureForIndex(int index) {
		return getStructureFromIndex(index).getCode();
    	//return keys[idx];
    }




    public static String getFileStructureNameForIndex(int index) {
    	return getStructureFromIndex(index).getSimpleName();
    	//return names[idx];
    }



    public static int getNumberOfFileStructures() {
    	return constantConversions.getFileStructureNames().getConstantList().size();
    	//return numberOfEntries;
    }

}
