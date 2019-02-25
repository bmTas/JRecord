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
      
package net.sf.JRecord.cgen.support;

import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.CsvParser.ICsvParserIds;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class Code2JRecordConstants {
	
	public static List<String> JAVA_TYPES = Arrays.asList("String", "short", "int", "long", "BigDecimal", "BigInteger");

    private static final String[] JAVA_TYPE_NAME = new String[Type.LAST_SYSTEM_TYPE];
    private static final String[] RECORD_TYPES = new String[20];
    private static final String[] IO_TYPE = new String[150];
    private static final String[] DIALECT = new String[150];
    private static final String[] SPLIT = new String[20];
    private static final String[] COPYBOOK_FORMAT = new String[20];
    private static final String[] PARSER_ID = new String[ICsvParserIds.BASIC_PARSER_QUOTE_BY_TYPE + 8];
  
    static {
    	Arrays.fill(JAVA_TYPE_NAME, null);
    	Arrays.fill(RECORD_TYPES, null);
    	Arrays.fill(IO_TYPE, null);
       	Arrays.fill(SPLIT, null);
       	Arrays.fill(COPYBOOK_FORMAT, null);
    }
    
    
//
//	public static String toConstant(StringBuilder b) {
//		b = Conversion.replace(new StringBuilder(b), "-", "_");
//		return b.toString().toUpperCase();
//    }
//    
//    
//
//	public static String toSqlName(String str) {
//		StringBuilder b = new StringBuilder(str.toLowerCase());
////		b = Conversion.replace(new StringBuilder(b), "-", "_");
//		boolean toUpper = true;
//		for (int i = 0; i < b.length(); i++) {
//			char ch = b.charAt(i);
//			switch (ch) {
//			case '-' :
//			case '_' :
//				b.setCharAt(i, '_');
//				toUpper = true;
//				break;
//			default:
//				if (toUpper) {
//					b.setCharAt(i, Character.toUpperCase(ch));
//					toUpper = false;
//				} 
//			}
//		}
//		return b.toString();
//    }
//    
//
//	public static String toSuffix(String str) {
//		StringBuilder b = new StringBuilder(str);
//		if (b == null || b.length() == 0) {
//			return "";
//		}
//		b.setCharAt(0, Character.toUpperCase(b.charAt(0)));
//		return b.toString();
//	}
//	
//	public static String toFieldName(String b) {
//		return toJavaId(b, 'f');
//	}
//	
//	public static String toClassName(String b) {
//		return toJavaId(b, 'c');
//	}
//
//	
//	private static String toJavaId(String str, char pref) {
//		StringBuilder b = new StringBuilder(str);
//		if (b == null || b.length() == 0) {
//			return "";
//		}
//		
//		if ((b.charAt(0) >= 'A' && b.charAt(0) <= 'Z')) {
//			b.setCharAt(0, Character.toLowerCase(b.charAt(0)));
//		} else if (b.charAt(0) < 'a' || b.charAt(0) > 'z') {
//			b.insert(0, pref);
//		}
//		return b.toString();
//	}
//
//	public static String toJavaId(boolean isCobol, String name) {
//		if (isCobol) {
//			return cobolName2JavaName(name);
//		}
//		return string2JavaId(name);
//	}
//	
//	public static String cobolName2JavaName(String cobolName) {
//		return convertToPgmId(cobolName.toLowerCase(), cobolName.toUpperCase(), false).toString();
//	}
//
//	
//	public static String string2JavaId(String name) {
//		return convertToPgmId(name, name.toUpperCase(), true).toString();
//	}
//
//
//
//	private static StringBuilder convertToPgmId(String name, String ucCobolName, boolean keepSeperators) {
//		int length = ucCobolName.length();
//		StringBuilder b = new StringBuilder(length);
//
//		boolean toUCase = false; 
//		char c;
//		
//		for (int i = 0; i < length; i++) {
//			c = name.charAt(i);
//			switch (c) {
//			case ':':
//			case ';':
//			case '*':
//			case '=':
//			case '+':
//			case '\'':
//			case '\"':
//			case '~':
//			case '!':
//			case '|':
//			case '@':
//			case '#':
//			case '$':
//			case '%':
//			case ')':
//			case '[':
//			case ']':
//				break;
//			case '(':
//			case ',':
//				b.append('_');
//				toUCase = false;
//				break;
//			case '.':
//			case ' ':
//			case '-':
//			case '_':
//				if (keepSeperators) {
//					b.append('_');
//				}
//				toUCase = true;
//				break;
//			default:
//				if (toUCase) {
//					b.append(ucCobolName.charAt(i));
//					toUCase = false;
//				} else {
//					b.append(c);
//				}
//			}
//		}
//		return b;
//	}
//	

	/**
	 * Get the JRecord-Constant Type name for a type. This is for use in code generation
	 * 
	 * @param type JRecord Type
	 * 
	 * @return JRecord-Constant 
	 */
	public static String getJRecordTypeName(int type) {
		initTypeNames();
		
		if (type < 0 || type > JAVA_TYPE_NAME.length || JAVA_TYPE_NAME[type] == null) {
			return Integer.toString(type);
		}
		
		return "Type." + JAVA_TYPE_NAME[type];
	}
	
	@SuppressWarnings("deprecation")
	private static void initTypeNames() {
		if (JAVA_TYPE_NAME [Type.ftHtmlField ] == null) {		
			JAVA_TYPE_NAME [Type.ftChar                     ] = "ftChar";
			JAVA_TYPE_NAME [Type.ftCharRightJust            ] = "ftCharRightJust";
			JAVA_TYPE_NAME [Type.ftCharNullTerminated       ] = "ftCharNullTerminated";
			JAVA_TYPE_NAME [Type.ftCharNullPadded           ] = "ftCharNullPadded";
			JAVA_TYPE_NAME [Type.ftCharNoTrim               ] = "ftCharNoTrim";
			
			JAVA_TYPE_NAME [Type.ftHex                      ] = "ftHex";
			JAVA_TYPE_NAME [Type.ftNumLeftJustified         ] = "ftNumLeftJustified";
			JAVA_TYPE_NAME [Type.ftNumRightJustified        ] = "ftNumRightJustified";
			JAVA_TYPE_NAME [Type.ftNumZeroPadded            ] = "ftNumZeroPadded";
			JAVA_TYPE_NAME [Type.ftAssumedDecimal           ] = "ftAssumedDecimal";
			JAVA_TYPE_NAME [Type.ftSignSeparateLead         ] = "ftSignSeparateLead";
			JAVA_TYPE_NAME [Type.ftSignSeparateTrail        ] = "ftSignSeparateTrail";
			JAVA_TYPE_NAME [Type.ftSignSepLeadActualDecimal ] = "ftSignSepLeadActualDecimal";
			JAVA_TYPE_NAME [Type.ftSignSepTrailActualDecimal] = "ftSignSepTrailActualDecimal";
			JAVA_TYPE_NAME [Type.ftDecimal                  ] = "ftDecimal";
			JAVA_TYPE_NAME [Type.ftBinaryInt                ] = "ftBinaryInt";
			JAVA_TYPE_NAME [Type.ftPostiveBinaryInt         ] = "ftPostiveBinaryInt";
			JAVA_TYPE_NAME [Type.ftFloat                    ] = "ftFloat";
			JAVA_TYPE_NAME [Type.ftDouble                   ] = "ftDouble";
			JAVA_TYPE_NAME [Type.ftNumAnyDecimal            ] = "ftNumAnyDecimal";
			JAVA_TYPE_NAME [Type.ftPositiveNumAnyDecimal    ] = "ftPositiveNumAnyDecimal";
			JAVA_TYPE_NAME [Type.ftBit                      ] = "ftBit";
			JAVA_TYPE_NAME [Type.ftAssumedDecimalPositive   ] = "ftAssumedDecimalPositive";
			JAVA_TYPE_NAME [Type.ftBinaryIntPositive        ] = "ftBinaryIntPositive";

			JAVA_TYPE_NAME [Type.ftNumZeroPaddedPN          ] = "ftNumZeroPaddedPN";
			JAVA_TYPE_NAME [Type.ftNumZeroPaddedPositive    ] = "ftNumZeroPaddedPositive";
			JAVA_TYPE_NAME [Type.ftNumCommaDecimal          ] = "ftNumCommaDecimal";
			JAVA_TYPE_NAME [Type.ftNumCommaDecimalPN        ] = "ftNumCommaDecimalPN";
			JAVA_TYPE_NAME [Type.ftNumCommaDecimalPositive  ] = "ftNumCommaDecimalPositive";

			JAVA_TYPE_NAME [Type.ftNumRightJustifiedPN      ] = "ftNumRightJustifiedPN";
			
			JAVA_TYPE_NAME [Type.ftPackedDecimal            ] = "ftPackedDecimal";
			JAVA_TYPE_NAME [Type.ftZonedNumeric             ] = "ftZonedNumeric";
			JAVA_TYPE_NAME [Type.ftPackedDecimalPostive     ] = "ftPackedDecimalPostive";
			JAVA_TYPE_NAME [Type.ftBinaryBigEndian          ] = "ftBinaryBigEndian";                                                      
			JAVA_TYPE_NAME [Type.ftBinaryBigEndianPositive  ] = "ftBinaryBigEndianPositive";
			JAVA_TYPE_NAME [Type.ftPositiveBinaryBigEndian  ] = "ftPositiveBinaryBigEndian";
			JAVA_TYPE_NAME [Type.ftRmComp                   ] = "ftRmComp";
			JAVA_TYPE_NAME [Type.ftRmCompPositive           ] = "ftRmCompPositive";
			
			JAVA_TYPE_NAME [Type.ftPackedDecimalSmall       ] = "ftPackedDecimal";          
			JAVA_TYPE_NAME [Type.ftPackedDecimalSmallPostive] = "ftPackedDecimalPostive   ";            
			JAVA_TYPE_NAME [Type.ftIntBigEndianSmall        ] = "ftBinaryBigEndian        ";            
			JAVA_TYPE_NAME [Type.ftIntBigEndianPositive     ] = "ftBinaryBigEndianPositive";            
			JAVA_TYPE_NAME [Type.ftUIntBigEndianSmall       ] = "ftPositiveBinaryBigEndian";            
			JAVA_TYPE_NAME [Type.ftIntSmall                 ] = "ftBinaryInt              ";            
			JAVA_TYPE_NAME [Type.ftIntPositiveSmall         ] = "ftBinaryIntPositive      ";            
			JAVA_TYPE_NAME [Type.ftUIntSmall                ] = "ftPostiveBinaryInt       ";   

			JAVA_TYPE_NAME [Type.ftFjZonedNumeric           ] = "ftFjZonedNumeric";
			JAVA_TYPE_NAME [Type.ftNumRightJustCommaDp      ] = "ftNumRightJustCommaDp";
			JAVA_TYPE_NAME [Type.ftNumRightJustCommaDpPN    ] = "ftNumRightJustCommaDpPN";

			JAVA_TYPE_NAME [Type.ftCharMultiLine            ] = "ftCharMultiLine";

			JAVA_TYPE_NAME [Type.ftDate                     ] = "ftDate";
			JAVA_TYPE_NAME [Type.ftDateYMD                  ] = "ftDateYMD";
			JAVA_TYPE_NAME [Type.ftDateYYMD                 ] = "ftDateYYMD";
			JAVA_TYPE_NAME [Type.ftDateDMY                  ] = "ftDateDMY";
			JAVA_TYPE_NAME [Type.ftDateDMYY                 ] = "ftDateDMYY";

			JAVA_TYPE_NAME [Type.ftCharRestOfFixedRecord    ] = "ftCharRestOfFixedRecord";
			JAVA_TYPE_NAME [Type.ftCharRestOfRecord         ] = "ftCharRestOfRecord";
			
//			javaTypeName [Type.ftProtoField               ] = "ftProtoField";
//			javaTypeName [Type.ftAvroField                ] = "ftAvroField";                                                                                                                                               
			JAVA_TYPE_NAME [Type.ftArrayField               ] = "ftArrayField";
			JAVA_TYPE_NAME [Type.ftComboItemField           ] = "ftComboItemField";                                                                         
//			javaTypeName [Type.ftAvroUnionField           ] = "ftAvroUnionField";
			
			JAVA_TYPE_NAME [Type.ftCheckBoxY                ] = "ftCheckBoxY";
			JAVA_TYPE_NAME [Type.ftCheckBoxTrue             ] = "ftCheckBoxTrue";
			JAVA_TYPE_NAME [Type.ftCheckBoxYN               ] = "ftCheckBoxYN";
			JAVA_TYPE_NAME [Type.ftCheckBoxTF               ] = "ftCheckBoxTF";
			JAVA_TYPE_NAME [Type.ftCheckBoxBoolean          ] = "ftCheckBoxBoolean";
			
			JAVA_TYPE_NAME [Type.ftCsvArray                 ] = "ftCsvArray";
			JAVA_TYPE_NAME [Type.ftXmlNameTag               ] = "ftXmlNameTag";
			JAVA_TYPE_NAME [Type.ftMultiLineEdit            ] = "ftMultiLineEdit";
			JAVA_TYPE_NAME [Type.ftMultiLineChar            ] = "ftMultiLineChar";
			JAVA_TYPE_NAME [Type.ftHtmlField                ] = "ftHtmlField";
		}
	}

	public static String getRecordTypeName(int recType) {
		
		if (RECORD_TYPES[Constants.rtFixedLengthRecords] == null) {
		    RECORD_TYPES[Constants.rtBinaryRecord         ] = "rtBinaryRecord";        
		    RECORD_TYPES[Constants.rtRecordLayout         ] = "rtRecordLayout";        
		    RECORD_TYPES[Constants.rtDelimited            ] = "rtDelimited";           
		    RECORD_TYPES[Constants.rtDelimitedAndQuote    ] = "rtDelimitedAndQuote";   
		    RECORD_TYPES[Constants.RT_XML                 ] = "RT_XML";                
		    RECORD_TYPES[Constants.rtGroupOfRecords       ] = "rtGroupOfRecords";      
		    RECORD_TYPES[Constants.rtGroupOfBinaryRecords ] = "rtGroupOfBinaryRecords";
		    RECORD_TYPES[Constants.rtFixedLengthRecords   ] = "rtFixedLengthRecords";
		}
		
		if (recType < 0 || recType > RECORD_TYPES.length || RECORD_TYPES[recType] == null) {
			return Integer.toString(recType);
		}
		
		return "Constants." + RECORD_TYPES[recType];
	}
	

	public static String getDialectName(int dialect) {
		
		if (DIALECT[ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL] == null) {
		    DIALECT[ICopybookDialects.FMT_INTEL]                   = "FMT_INTEL";
		    DIALECT[ICopybookDialects.FMT_MAINFRAME]               = "FMT_MAINFRAME";
		    DIALECT[ICopybookDialects.FMT_FUJITSU]                 = "FMT_FUJITSU";
		    DIALECT[ICopybookDialects.FMT_BIG_ENDIAN]              = "FMT_BIG_ENDIAN";
		    DIALECT[ICopybookDialects.FMT_GNU_COBOL]               = "FMT_GNU_COBOL";
		    DIALECT[ICopybookDialects.FMT_FS2000]                  = "FMT_FS2000";
		    DIALECT[ICopybookDialects.FMT_GNU_COBOL_MVS]           = "FMT_GNU_COBOL_MVS";
		    DIALECT[ICopybookDialects.FMT_GNU_COBOL_MF]            = "FMT_GNU_COBOL_MF";
		    DIALECT[ICopybookDialects.FMT_GNU_COBOL_BE]            = "FMT_GNU_COBOL_BE";
		    DIALECT[ICopybookDialects.FMT_FS2000_BE]               = "FMT_FS2000_BE";                                                           
		    DIALECT[ICopybookDialects.FMT_GNU_COBOL_MVS_BE]        = "FMT_GNU_COBOL_MVS_BE";
		    DIALECT[ICopybookDialects.FMT_OC_MICRO_FOCUS_BE]       = "FMT_OC_MICRO_FOCUS_BE";
		    DIALECT[ICopybookDialects.FMT_MICRO_FOCUS]             = "FMT_MICRO_FOCUS";                                           
		    DIALECT[ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL] = "FMT_MAINFRAME_COMMA_DECIMAL";
		    DIALECT[ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL]   = "FMT_FUJITSU_COMMA_DECIMAL";
		}
		
		if (dialect < 0 || dialect > DIALECT.length || DIALECT[dialect] == null) {
			return Integer.toString(dialect);
		}
		
		return "ICopybookDialects." + DIALECT[dialect];
	}
	

	public static String getSplitName(int split) {
		
		if (RECORD_TYPES[RECORD_TYPES.length - 1] == null) {
		    SPLIT[ICobolSplitOptions.SPLIT_NONE]              = "SPLIT_NONE";
		    SPLIT[ICobolSplitOptions.SPLIT_REDEFINE]          = "SPLIT_REDEFINE";
		    SPLIT[ICobolSplitOptions.SPLIT_01_LEVEL]          = "SPLIT_01_LEVEL";
		    SPLIT[ICobolSplitOptions.SPLIT_HIGHEST_REPEATING] = "SPLIT_HIGHEST_REPEATING";
		}
		
		if (split < 0 || split > SPLIT.length || SPLIT[split] == null) {
			return Integer.toString(split);
		}
		
		return "ICobolSplitOptions." + SPLIT[split];
	}


	public static String getCopybookFormatName(int format) {
		
		if (RECORD_TYPES[RECORD_TYPES.length - 1] == null) {
		    COPYBOOK_FORMAT[Cb2xmlConstants.USE_STANDARD_COLUMNS ]  = "USE_STANDARD_COLUMNS";
		    COPYBOOK_FORMAT[Cb2xmlConstants.USE_SUPPLIED_COLUMNS ]  = "USE_SUPPLIED_COLUMNS";
		    COPYBOOK_FORMAT[Cb2xmlConstants.USE_COLS_6_TO_80     ]  = "USE_COLS_6_TO_80";                           
		    COPYBOOK_FORMAT[Cb2xmlConstants.USE_LONG_LINE        ]  = "USE_LONG_LINE";
		    COPYBOOK_FORMAT[Cb2xmlConstants.USE_PROPERTIES_FILE  ]  = "USE_PROPERTIES_FILE";
		    COPYBOOK_FORMAT[Cb2xmlConstants.FREE_FORMAT          ]  = "FREE_FORMAT";           
		}
		
		if (format < 0 || format > COPYBOOK_FORMAT.length || COPYBOOK_FORMAT[format] == null) {
			return Integer.toString(format);
		}
		
		return "Cb2xmlConstants." + COPYBOOK_FORMAT[format];
	}


	public static String getJRecordIoTypeName(int type) {
		initIoTypes();
		
		if (type < 0 || type > IO_TYPE.length || IO_TYPE[type] == null) {
			return Integer.toString(type);
		}
		
		return "Constants." + IO_TYPE[type];
	}

	@SuppressWarnings("deprecation")
	private static void initIoTypes() {

		if (IO_TYPE [Constants.IO_PROTO_SD_SINGLE_MESSAGE] == null) {
		    IO_TYPE [Constants.IO_DEFAULT              ] = "IO_DEFAULT";
		    IO_TYPE [Constants.IO_STANDARD_TEXT_FILE   ] = "IO_STANDARD_TEXT_FILE";
		    IO_TYPE [Constants.IO_FIXED_LENGTH_RECORDS ] = "IO_FIXED_LENGTH_RECORDS";
		    IO_TYPE [Constants.IO_FIXED_BYTE_ENTER_FONT] = "IO_FIXED_LENGTH_RECORDS";
		    IO_TYPE [Constants.IO_BINARY_IBM_4680      ] = "IO_BINARY_IBM_4680";
		    IO_TYPE [Constants.IO_VB                   ] = "IO_VB";
		    IO_TYPE [Constants.IO_VBS                  ] = "IO_VBS";
		    IO_TYPE [Constants.IO_VB_DUMP              ] = "IO_VB_DUMP";
		    IO_TYPE [Constants.IO_VB_DUMP2             ] = "IO_VB_DUMP2";
		    IO_TYPE [Constants.IO_VB_FUJITSU           ] = "IO_VB_FUJITSU";
		    IO_TYPE [Constants.IO_VB_GNU_COBOL         ] = "IO_VB_GNU_COBOL";
		    IO_TYPE [Constants.IO_BIN_TEXT             ] = "IO_BIN_TEXT";
		    IO_TYPE [Constants.IO_TEXT_BYTE_ENTER_FONT ] = "IO_BIN_TEXT";
		    IO_TYPE [Constants.IO_FIXED_LENGTH_CHAR    ] = "IO_FIXED_LENGTH_CHAR";
		    IO_TYPE [Constants.IO_FIXED_CHAR_ENTER_FONT] = "IO_FIXED_LENGTH_CHAR";
		    IO_TYPE [Constants.IO_UNKOWN_FORMAT        ] = "IO_UNKOWN_FORMAT";
		    IO_TYPE [Constants.IO_WIZARD               ] = "IO_WIZARD";                                         
		    IO_TYPE [Constants.IO_MICROFOCUS           ] = "IO_MICROFOCUS";
		    IO_TYPE [Constants.IO_CSV                  ] = "IO_CSV";
		    IO_TYPE [Constants.IO_BIN_CSV              ] = "IO_BIN_CSV";
		    IO_TYPE [Constants.IO_UNICODE_CSV          ] = "IO_UNICODE_CSV";
		    IO_TYPE [Constants.IO_CSV_NAME_1ST_LINE    ] = "IO_CSV_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_BIN_CSV_NAME_1ST_LINE] = "IO_BIN_CSV_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_NAME_1ST_LINE        ] = "IO_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_GENERIC_CSV          ] = "IO_GENERIC_CSV";
		    IO_TYPE [Constants.IO_BIN_NAME_1ST_LINE    ] = "IO_BIN_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_UNICODE_NAME_1ST_LINE] = "IO_UNICODE_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_XML_USE_LAYOUT       ] = "IO_XML_USE_LAYOUT";
		    IO_TYPE [Constants.IO_XML_BUILD_LAYOUT     ] = "IO_XML_BUILD_LAYOUT";
		    IO_TYPE [Constants.IO_PROTO_DELIMITED      ] = "IO_PROTO_DELIMITED";
		    IO_TYPE [Constants.IO_PROTO_SINGLE_MESSAGE ] = "IO_PROTO_SINGLE_MESSAGE";
		    IO_TYPE [Constants.IO_PROTO_SD_DELIMITED   ] = "IO_PROTO_SD_DELIMITED";
		    IO_TYPE [Constants.IO_THRIFT_FILE          ] = "IO_THRIFT_FILE";
		    IO_TYPE [Constants.IO_AVRO_FILE            ] = "IO_AVRO_FILE";
		    IO_TYPE [Constants.IO_GETTEXT_PO           ] = "IO_GETTEXT_PO";
		    IO_TYPE [Constants.IO_TIP                  ] = "IO_TIP";
		    IO_TYPE [Constants.IO_CONTINOUS_NO_LINE_MARKER   ] = "IO_CONTINOUS_NO_LINE_MARKER";
		    IO_TYPE [Constants.IO_UNICODE_CSV_NAME_1ST_LINE  ] = "IO_UNICODE_CSV_NAME_1ST_LINE";
		    IO_TYPE [Constants.IO_STANDARD_UNICODE_TEXT_FILE ] = "IO_STANDARD_UNICODE_TEXT_FILE";
		    IO_TYPE [Constants.IO_TEXT_CHAR_ENTER_FONT       ] = "IO_STANDARD_UNICODE_TEXT_FILE";
		    IO_TYPE [Constants.IO_PROTO_SD_SINGLE_MESSAGE    ] = "IO_PROTO_SD_SINGLE_MESSAGE";
		}
	}
	
	public static String getParserName(int parserId) {
		
		initParserIds();
		
		return parserId < 0 || parserId >= PARSER_ID.length || PARSER_ID[parserId] == null
					? Integer.toString(parserId)
					: "ICsvParserIds." + PARSER_ID[parserId];
	}
	
	private static void initParserIds() {
		if (PARSER_ID[ICsvParserIds.BASIC_PARSER_QUOTE_BY_TYPE] == null) {
			PARSER_ID[ICsvParserIds.EXTENDED_BASIC_CSV_PARSER				] ="EXTENDED_BASIC_CSV_PARSER";
			PARSER_ID[ICsvParserIds.STANDARD_CSV_PARSER						] ="STANDARD_CSV_PARSER";
			PARSER_ID[ICsvParserIds.DB_CSV_PARSER							] ="DB_CSV_PARSER";
			PARSER_ID[ICsvParserIds.BASIC_QUOTED_COL_NAME_CSV_PARSER		] ="BASIC_QUOTED_COL_NAME_CSV_PARSER";
			PARSER_ID[ICsvParserIds.STANDARD_QUOTED_COL_NAME_CSV_PARSER		] ="STANDARD_QUOTED_COL_NAME_CSV_PARSER";
			PARSER_ID[ICsvParserIds.DB_QUOTED_COL_NAME_CSV_PARSER			] ="DB_QUOTED_COL_NAME_CSV_PARSER";
			PARSER_ID[ICsvParserIds.BASIC_ENSURE_CORRECT_NO_FIELDS			] ="BASIC_ENSURE_CORRECT_NO_FIELDS";
			PARSER_ID[ICsvParserIds.BASIC_ENSURE_CORRECT_NO_FIELDS_P1		] ="BASIC_ENSURE_CORRECT_NO_FIELDS_P1";
			PARSER_ID[ICsvParserIds.BASIC_EMBEDDED_CR						] ="BASIC_EMBEDDED_CR";
			PARSER_ID[ICsvParserIds.STANDARD_EMBEDDED_CR					] ="STANDARD_EMBEDDED_CR";
			PARSER_ID[ICsvParserIds.BASIC_EMBEDDED_CR_NAMES_IN_QUOTES		] ="BASIC_EMBEDDED_CR_NAMES_IN_QUOTES";
			PARSER_ID[ICsvParserIds.STANDARD_EMBEDDED_CR_NAMES_INQUOTE		] ="STANDARD_EMBEDDED_CR_NAMES_INQUOTE";
			PARSER_ID[ICsvParserIds.STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE 	] ="STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE";
			PARSER_ID[ICsvParserIds.BASIC_CSV_PARSER_NEW_NUM				] ="BASIC_CSV_PARSER_NEW_NUM";
			PARSER_ID[ICsvParserIds.BASIC_PARSER_QUOTE_BY_TYPE				] ="BASIC_PARSER_QUOTE_BY_TYPE";

		}
	}
	
	
	@SuppressWarnings("deprecation")
	public static String typeToJavaType(boolean csv, int typeId, int length, int decimal) {
		if (! TypeManager.isNumeric(typeId)) {
			return "String";
		} else if (typeId == Type.ftFloat) {
			return "float";
		} else if (typeId == Type.ftDouble) {
			return "double";
		} else if (decimal > 0 || typeId == Type.ftNumAnyDecimal) {
			return "BigDecimal";
		} else if (csv) {
			return "long";
		}
		
		if (typeId == Type.ftPackedDecimal || typeId == Type.ftPackedDecimalSmall
		||  typeId == Type.ftPackedDecimalPostive || typeId == Type.ftPackedDecimalSmallPostive) {
			if (length < 3) {
				return "short";
			} else if (length < 6) {
				return "int";
			} else if (length < 11) {
				return "long";
			}

			return "BigInteger";
		} else if (TypeManager.isBinary(typeId)) {
			if (length < 3) {
				return "short";
			} else if (length < 5) {
				return "int";
			} else if (length < 9) {
				return "long";
			}			
			return "BigInteger";
		}
		
		if (length < 5) {
			return "short";
		} else if (length < 10) {
			return "int";
		} else if (length < 19) {
			return "long";
		}			
		return "BigInteger";

	}
}
