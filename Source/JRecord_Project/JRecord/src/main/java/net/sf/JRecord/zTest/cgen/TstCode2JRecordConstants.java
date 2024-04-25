package net.sf.JRecord.zTest.cgen;

import static org.junit.Assert.*;

import org.junit.Test;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.cgen.support.Code2JRecordConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class TstCode2JRecordConstants {

	@Test
	public void testCopybookFormat() {
		String[] copybookFormat = new String[20];
		
	    copybookFormat[Cb2xmlConstants.USE_STANDARD_COLUMNS ]  = "USE_STANDARD_COLUMNS";
	    copybookFormat[Cb2xmlConstants.USE_SUPPLIED_COLUMNS ]  = "USE_SUPPLIED_COLUMNS";
	    copybookFormat[Cb2xmlConstants.USE_COLS_6_TO_80     ]  = "USE_COLS_6_TO_80";                           
	    copybookFormat[Cb2xmlConstants.USE_LONG_LINE        ]  = "USE_LONG_LINE";
	    copybookFormat[Cb2xmlConstants.USE_PROPERTIES_FILE  ]  = "USE_PROPERTIES_FILE";
	    copybookFormat[Cb2xmlConstants.FREE_FORMAT          ]  = "FREE_FORMAT";           

	    for (int i = 0; i < copybookFormat.length; i++) {
	    	if (copybookFormat[i] != null) {
	    		assertEquals("Cb2xmlConstants."+ copybookFormat[i], Code2JRecordConstants.getCopybookFormatName(i));
	    	} else {
	    		assertEquals(""+ i, Code2JRecordConstants.getCopybookFormatName(i));
	    	}
	    }
	}

	@Test
	public void testSplit() {
		String[] split = new String[10];
		
	    split[ICobolSplitOptions.SPLIT_NONE]              = "SPLIT_NONE";
	    split[ICobolSplitOptions.SPLIT_REDEFINE]          = "SPLIT_REDEFINE";
	    split[ICobolSplitOptions.SPLIT_01_LEVEL]          = "SPLIT_01_LEVEL";
	    split[ICobolSplitOptions.SPLIT_HIGHEST_REPEATING] = "SPLIT_HIGHEST_REPEATING";
	    split[ICobolSplitOptions.SPLIT_TOP_LEVEL]         = "SPLIT_TOP_LEVEL";

	    for (int i = 0; i < split.length; i++) {
	    	if (split[i] != null) {
	    		assertEquals("ICobolSplitOptions."+ split[i], Code2JRecordConstants.getSplitName(i));
	    	} else {
	    		assertEquals(i + "", Code2JRecordConstants.getSplitName(i));
	    		
	    	}
	    }
	}
	

	@Test
	public void testDialect() {
		String[] dialect = new String[50];
		
	    dialect[ICopybookDialects.FMT_INTEL]                   = "FMT_INTEL";
	    dialect[ICopybookDialects.FMT_MAINFRAME]               = "FMT_MAINFRAME";
	    dialect[ICopybookDialects.FMT_FUJITSU]                 = "FMT_FUJITSU";
	    dialect[ICopybookDialects.FMT_BIG_ENDIAN]              = "FMT_BIG_ENDIAN";
	    dialect[ICopybookDialects.FMT_GNU_COBOL]               = "FMT_GNU_COBOL";
	    
	    dialect[ICopybookDialects.FMT_FS2000]                  = "FMT_FS2000";
	    dialect[ICopybookDialects.FMT_GNU_COBOL_MVS]           = "FMT_GNU_COBOL_MVS";
	    dialect[ICopybookDialects.FMT_GNU_COBOL_MF]            = "FMT_GNU_COBOL_MF";
	    dialect[ICopybookDialects.FMT_GNU_COBOL_BE]            = "FMT_GNU_COBOL_BE";
	    dialect[ICopybookDialects.FMT_FS2000_BE]               = "FMT_FS2000_BE";                                                           
	   
	    dialect[ICopybookDialects.FMT_GNU_COBOL_MVS_BE]        = "FMT_GNU_COBOL_MVS_BE";
	    dialect[ICopybookDialects.FMT_OC_MICRO_FOCUS_BE]       = "FMT_OC_MICRO_FOCUS_BE";
//	    dialect[ICopybookDialects.FMT_MICRO_FOCUS]             = "FMT_MICRO_FOCUS";                                           
	    dialect[ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL] = "FMT_MAINFRAME_COMMA_DECIMAL";
	    dialect[ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL]   = "FMT_FUJITSU_COMMA_DECIMAL";

	    for (int i = 0; i < dialect.length; i++) {
	    	String dialectName = Code2JRecordConstants.getDialectName(i);
			if (dialect[i] != null) {
	    		assertEquals("ICopybookDialects."+ dialect[i], dialectName);
	    	} else {
	    		assertEquals(i + "", dialectName);
	    		
	    	}
	    }
	}

	@Test
	public void testRecordTypes() {
		String[] recordTypes = new String[25];
		
	    recordTypes[Constants.rtBinaryRecord         ] = "rtBinaryRecord";        
	    recordTypes[Constants.rtRecordLayout         ] = "rtRecordLayout";        
	    recordTypes[Constants.rtDelimited            ] = "rtDelimited";           
	    recordTypes[Constants.rtDelimitedAndQuote    ] = "rtDelimitedAndQuote";   
	    recordTypes[Constants.RT_XML                 ] = "RT_XML";                
	    recordTypes[Constants.rtGroupOfRecords       ] = "rtGroupOfRecords";      
	    recordTypes[Constants.rtGroupOfBinaryRecords ] = "rtGroupOfBinaryRecords";
	    recordTypes[Constants.rtFixedLengthRecords   ] = "rtFixedLengthRecords";

	    for (int i = 0; i < recordTypes.length; i++) {
	    	String recordTypeName = Code2JRecordConstants.getRecordTypeName(i);
			if (recordTypes[i] != null) {
	    		assertEquals("Constants."+ recordTypes[i], recordTypeName);
	    	} else {
	    		assertEquals(i + "", recordTypeName);
	    		
	    	}
	    }
	}

	@Test
	public void testIOTypes() {
		String[] ioTypes = new String[160];
		
	    ioTypes [IFileStructureConstants.IO_DEFAULT              ] = "IO_DEFAULT";
	    ioTypes [IFileStructureConstants.IO_STANDARD_TEXT_FILE   ] = "IO_STANDARD_TEXT_FILE";
	    ioTypes [IFileStructureConstants.IO_FIXED_LENGTH_RECORDS ] = "IO_FIXED_LENGTH_RECORDS";
	    ioTypes [IFileStructureConstants.IO_FIXED_BYTE_ENTER_FONT] = "IO_FIXED_BYTE_ENTER_FONT";
	    ioTypes [IFileStructureConstants.IO_BINARY_IBM_4680      ] = "IO_BINARY_IBM_4680";
	    ioTypes [IFileStructureConstants.IO_VB                   ] = "IO_VB";
	    ioTypes [IFileStructureConstants.IO_VBS                  ] = "IO_VBS";
	    ioTypes [IFileStructureConstants.IO_VB_DUMP              ] = "IO_VB_DUMP";
	    ioTypes [IFileStructureConstants.IO_VB_DUMP2             ] = "IO_VB_DUMP2";
	    ioTypes [IFileStructureConstants.IO_VB_FUJITSU           ] = "IO_VB_FUJITSU";
	    ioTypes [IFileStructureConstants.IO_VB_GNU_COBOL         ] = "IO_VB_GNU_COBOL";
	    ioTypes [IFileStructureConstants.IO_BIN_TEXT             ] = "IO_BIN_TEXT";
	    ioTypes [IFileStructureConstants.IO_TEXT_BYTE_ENTER_FONT ] = "IO_TEXT_BYTE_ENTER_FONT";
	    ioTypes [IFileStructureConstants.IO_FIXED_LENGTH_CHAR    ] = "IO_FIXED_LENGTH_CHAR";
	    ioTypes [IFileStructureConstants.IO_FIXED_CHAR_ENTER_FONT] = "IO_FIXED_CHAR_ENTER_FONT";
	    ioTypes [IFileStructureConstants.IO_UNKOWN_FORMAT        ] = "IO_UNKOWN_FORMAT";
	    ioTypes [IFileStructureConstants.IO_WIZARD               ] = "IO_WIZARD";                                         
	    ioTypes [IFileStructureConstants.IO_MICROFOCUS           ] = "IO_MICROFOCUS";
	    ioTypes [IFileStructureConstants.IO_CSV                  ] = "IO_CSV";
	    ioTypes [IFileStructureConstants.IO_BIN_CSV              ] = "IO_BIN_CSV";
	    ioTypes [IFileStructureConstants.IO_UNICODE_CSV          ] = "IO_UNICODE_CSV";
	    ioTypes [IFileStructureConstants.IO_CSV_NAME_1ST_LINE    ] = "IO_CSV_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_BIN_CSV_NAME_1ST_LINE] = "IO_BIN_CSV_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_NAME_1ST_LINE        ] = "IO_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_GENERIC_CSV          ] = "IO_GENERIC_CSV";
	    ioTypes [IFileStructureConstants.IO_BIN_NAME_1ST_LINE    ] = "IO_BIN_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE] = "IO_UNICODE_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_XML_USE_LAYOUT       ] = "IO_XML_USE_LAYOUT";
	    ioTypes [IFileStructureConstants.IO_XML_BUILD_LAYOUT     ] = "IO_XML_BUILD_LAYOUT";
//	    ioTypes [IFileStructureConstants.IO_PROTO_DELIMITED      ] = "IO_PROTO_DELIMITED";
//	    ioTypes [IFileStructureConstants.IO_PROTO_SINGLE_MESSAGE ] = "IO_PROTO_SINGLE_MESSAGE";
//	    ioTypes [IFileStructureConstants.IO_PROTO_SD_DELIMITED   ] = "IO_PROTO_SD_DELIMITED";
//	    ioTypes [IFileStructureConstants.IO_THRIFT_FILE          ] = "IO_THRIFT_FILE";
//	    ioTypes [IFileStructureConstants.IO_AVRO_FILE            ] = "IO_AVRO_FILE";
	    ioTypes [IFileStructureConstants.IO_GETTEXT_PO           ] = "IO_GETTEXT_PO";
	    ioTypes [IFileStructureConstants.IO_TIP                  ] = "IO_TIP";
	    ioTypes [IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER   ] = "IO_CONTINOUS_NO_LINE_MARKER";
	    ioTypes [IFileStructureConstants.IO_UNICODE_CSV_NAME_1ST_LINE  ] = "IO_UNICODE_CSV_NAME_1ST_LINE";
	    ioTypes [IFileStructureConstants.IO_STANDARD_UNICODE_TEXT_FILE ] = "IO_STANDARD_UNICODE_TEXT_FILE";
	    ioTypes [IFileStructureConstants.IO_TEXT_CHAR_ENTER_FONT       ] = "IO_TEXT_CHAR_ENTER_FONT";
//	    ioTypes [IFileStructureConstants.IO_PROTO_SD_SINGLE_MESSAGE    ] = "IO_PROTO_SD_SINGLE_MESSAGE";
	    
	    for (int i = 0; i < ioTypes.length; i++) {
	    	String ioTypeName = Code2JRecordConstants.getJRecordIoTypeName(i);
			if (ioTypes[i] != null) {
	    		assertEquals("IFileStructureConstants."+ ioTypes[i], ioTypeName);
	    	} else {
	    		assertEquals(i + "", ioTypeName);
	    		
	    	}
	    }
	}

	@Test
	public void testTypes() {
		String[] typeNames = new String[Type.LAST_SYSTEM_TYPE];
		typeNames [Type.ftChar                     ] = "ftChar";
		typeNames [Type.ftCharRightJust            ] = "ftCharRightJust";
		typeNames [Type.ftCharNullTerminated       ] = "ftCharNullTerminated";
		typeNames [Type.ftCharNullPadded           ] = "ftCharNullPadded";
		typeNames [Type.ftCharNoTrim               ] = "ftCharNoTrim";
		
		typeNames [Type.ftHex                      ] = "ftHex";
		typeNames [Type.ftNumLeftJustified         ] = "ftNumLeftJustified";
		typeNames [Type.ftNumRightJustified        ] = "ftNumRightJustified";
		typeNames [Type.ftNumZeroPadded            ] = "ftNumZeroPadded";
		typeNames [Type.ftAssumedDecimal           ] = "ftAssumedDecimal";
		typeNames [Type.ftSignSeparateLead         ] = "ftSignSeparateLead";
		typeNames [Type.ftSignSeparateTrail        ] = "ftSignSeparateTrail";
		typeNames [Type.ftSignSepLeadActualDecimal ] = "ftSignSepLeadActualDecimal";
		typeNames [Type.ftSignSepTrailActualDecimal] = "ftSignSepTrailActualDecimal";
		typeNames [Type.ftDecimal                  ] = "ftDecimal";
		typeNames [Type.ftBinaryInt                ] = "ftBinaryInt";
		typeNames [Type.ftPostiveBinaryInt         ] = "ftPostiveBinaryInt";
		typeNames [Type.ftFloat                    ] = "ftFloat";
		typeNames [Type.ftDouble                   ] = "ftDouble";
		typeNames [Type.ftNumAnyDecimal            ] = "ftNumAnyDecimal";
		typeNames [Type.ftPositiveNumAnyDecimal    ] = "ftPositiveNumAnyDecimal";
		typeNames [Type.ftBit                      ] = "ftBit";
		typeNames [Type.ftAssumedDecimalPositive   ] = "ftAssumedDecimalPositive";
		typeNames [Type.ftBinaryIntPositive        ] = "ftBinaryIntPositive";

		typeNames [Type.ftNumZeroPaddedPN          ] = "ftNumZeroPaddedPN";
		typeNames [Type.ftNumZeroPaddedPositive    ] = "ftNumZeroPaddedPositive";
		typeNames [Type.ftNumCommaDecimal          ] = "ftNumCommaDecimal";
		typeNames [Type.ftNumCommaDecimalPN        ] = "ftNumCommaDecimalPN";
		typeNames [Type.ftNumCommaDecimalPositive  ] = "ftNumCommaDecimalPositive";

		typeNames [Type.ftNumRightJustifiedPN      ] = "ftNumRightJustifiedPN";
		
		typeNames [Type.ftPackedDecimal            ] = "ftPackedDecimal";
		typeNames [Type.ftZonedNumeric             ] = "ftZonedNumeric";
		typeNames [Type.ftPackedDecimalPostive     ] = "ftPackedDecimalPostive";
		typeNames [Type.ftBinaryBigEndian          ] = "ftBinaryBigEndian";                                                      
		typeNames [Type.ftBinaryBigEndianPositive  ] = "ftBinaryBigEndianPositive";
		typeNames [Type.ftPositiveBinaryBigEndian  ] = "ftPositiveBinaryBigEndian";
		typeNames [Type.ftRmComp                   ] = "ftRmComp";
		typeNames [Type.ftRmCompPositive           ] = "ftRmCompPositive";
		
		typeNames [Type.ftPackedDecimalSmall       ] = "ftPackedDecimal";          
		typeNames [Type.ftPackedDecimalSmallPostive] = "ftPackedDecimalPostive   ";            
		typeNames [Type.ftIntBigEndianSmall        ] = "ftBinaryBigEndian        ";            
		typeNames [Type.ftIntBigEndianPositive     ] = "ftBinaryBigEndianPositive";            
		typeNames [Type.ftUIntBigEndianSmall       ] = "ftPositiveBinaryBigEndian";            
		typeNames [Type.ftIntSmall                 ] = "ftBinaryInt              ";            
		typeNames [Type.ftIntPositiveSmall         ] = "ftBinaryIntPositive      ";            
		typeNames [Type.ftUIntSmall                ] = "ftPostiveBinaryInt       ";   

		typeNames [Type.ftFjZonedNumeric           ] = "ftFjZonedNumeric";
		typeNames [Type.ftNumRightJustCommaDp      ] = "ftNumRightJustCommaDp";
		typeNames [Type.ftNumRightJustCommaDpPN    ] = "ftNumRightJustCommaDpPN";

		typeNames [Type.ftCharMultiLine            ] = "ftCharMultiLine";

		typeNames [Type.ftDate                     ] = "ftDate";
		typeNames [Type.ftDateYMD                  ] = "ftDateYMD";
		typeNames [Type.ftDateYYMD                 ] = "ftDateYYMD";
		typeNames [Type.ftDateDMY                  ] = "ftDateDMY";
		typeNames [Type.ftDateDMYY                 ] = "ftDateDMYY";

		typeNames [Type.ftCharRestOfFixedRecord    ] = "ftCharRestOfFixedRecord";
		typeNames [Type.ftCharRestOfRecord         ] = "ftCharRestOfRecord";
		
//		javaTypeName [Type.ftProtoField               ] = "ftProtoField";
//		javaTypeName [Type.ftAvroField                ] = "ftAvroField";                                                                                                                                               
		typeNames [Type.ftArrayField               ] = "ftArrayField";
		typeNames [Type.ftComboItemField           ] = "ftComboItemField";                                                                         
//		javaTypeName [Type.ftAvroUnionField           ] = "ftAvroUnionField";
		
		typeNames [Type.ftCheckBoxY                ] = "ftCheckBoxY";
		typeNames [Type.ftCheckBoxTrue             ] = "ftCheckBoxTrue";
		typeNames [Type.ftCheckBoxYN               ] = "ftCheckBoxYN";
		typeNames [Type.ftCheckBoxTF               ] = "ftCheckBoxTF";
		typeNames [Type.ftCheckBoxBoolean          ] = "ftCheckBoxBoolean";
		
		typeNames [Type.ftCsvArray                 ] = "ftCsvArray";
		typeNames [Type.ftXmlNameTag               ] = "ftXmlNameTag";
		typeNames [Type.ftMultiLineEdit            ] = "ftMultiLineEdit";
		typeNames [Type.ftMultiLineChar            ] = "ftMultiLineChar";
		typeNames [Type.ftHtmlField                ] = "ftHtmlField";
		typeNames [Type.ftGnuCblZonedNumeric] = "ftGnuCblZonedNumeric";

	    for (int i = 0; i < typeNames.length; i++) {
	    	String typeName = Code2JRecordConstants.getJRecordTypeName(i);
			if (typeNames[i] != null) {
	    		assertEquals("" + i, "Type."+ typeNames[i], typeName);
	    	} else if (i < Type.ftRecordEditorType ||  i > Type.ftZonedAsciiSmallPositive) {
	    		assertEquals(i + "", typeName);  		
	    	}
	    }

	}

}
