package net.sf.JRecord.constantNames;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Types.Type;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class ConstantNames {
	private static final ConstantConversions constantConversions = new ConstantConversions();
	
	/**
	 * @return the constant conversions
	 */
	public static ConstantConversions getConstantConversions() {
		return constantConversions;
	}

	public static ConstantNameConversion getFileStructureNames() {
		return constantConversions.getFileStructureNames();
	}
	
	/**
	 * @return
	 * @see net.sf.JRecord.constantNames.ConstantNames.ConstantConversions#getTypeNames()
	 */
	public static ConstantNameConversion getTypeNames() {
		return constantConversions.getTypeNames();
	}

	public static class ConstantConversions {
		private ConstantNameConversion fileStructures, typeNames, dialects, splitOptions, recordTypes, copybookFormats;
		
		private ConstantConversions() { }
	
		
		@SuppressWarnings("deprecation")
		public ConstantNameConversion getFileStructureNames() {
			if (fileStructures == null) {
				fileStructures = new ConstantNameConversion(IFileStructureConstants.class, new ConstantDetails[] {
						new ConstantDetails(IFileStructureConstants.IO_DEFAULT, "Default", "Default", "IO_DEFAULT"),
						new ConstantDetails(IFileStructureConstants.IO_STANDARD_TEXT_FILE, "Text IO", "Text", "IO_STANDARD_TEXT_FILE")
								.setExtraNames("IO_TEXT_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_FIXED_LENGTH_RECORDS, "Fixed Length Binary", "Fixed_Length", "IO_FIXED_LENGTH_RECORDS")
								.setExtraNames("IO_FIXED_LENGTH"),
						new ConstantDetails(IFileStructureConstants.IO_BINARY_IBM_4680, "Line based Binary", "Binary", "IO_BINARY_IBM_4680"),
						new ConstantDetails(IFileStructureConstants.IO_VB, "Mainframe VB (rdw based) Binary", "Mainframe_VB", "IO_VB"),
						new ConstantDetails(IFileStructureConstants.IO_VB_DUMP, "Mainframe VB Dump: includes Block length", "Mainframe_VB_As_RECFMU", "IO_VB_DUMP"),
						new ConstantDetails(IFileStructureConstants.IO_UNKOWN_FORMAT, "Unknown File Format", "UNKOWN_FORMAT", "IO_UNKOWN_FORMAT"),
						new ConstantDetails(IFileStructureConstants.IO_WIZARD, "File Wizard", "FILE_WIZARD", "IO_WIZARD"),
						new ConstantDetails(IFileStructureConstants.IO_VB_FUJITSU, "Fujitsu Variable Binary", "FUJITSU_VB", "IO_VB_FUJITSU"),
						new ConstantDetails(IFileStructureConstants.IO_VB_GNU_COBOL, "GNU Cobol VB", "Gnu_Cobol_VB", "IO_VB_GNU_COBOL"),
//						new ConstantDetails(IFileStructureConstants.IO_VB_OPEN_COBOL, "GNU Cobol VB", "Gnu_Cobol_VB", "IO_VB_OPEN_COBOL"),
						new ConstantDetails(IFileStructureConstants.IO_BIN_TEXT, "Text IO (byte Based)", "Byte_Text", "IO_BIN_TEXT"),
						new ConstantDetails(IFileStructureConstants.IO_FIXED_LENGTH_CHAR, "Fixed Length Char", "Fixed_Length_Char", "IO_FIXED_LENGTH_CHAR"),
						new ConstantDetails(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER, "Continuous no eol marker", "Continuous", "IO_CONTINOUS_NO_LINE_MARKER"),
						new ConstantDetails(IFileStructureConstants.IO_VBS, "Variable Block Spanned (VBS)", "Mainframe_VBS", "IO_VBS"),
						new ConstantDetails(IFileStructureConstants.IO_VB_DUMP2, "VB_DUMP2", "VB_DUMP2", "IO_VB_DUMP2"),
						new ConstantDetails(IFileStructureConstants.IO_MICROFOCUS, "Experimental Microfocus Header File", "Microfocus_Format", "IO_MICROFOCUS"),
						new ConstantDetails(IFileStructureConstants.IO_FIXED_BYTE_ENTER_FONT, "Fixed Byte, enter font", "FIXED_BYTE_ENTER_FONT", "IO_FIXED_BYTE_ENTER_FONT"),
						new ConstantDetails(IFileStructureConstants.IO_FIXED_CHAR_ENTER_FONT, "Fixed Char, enter font", "FIXED_CHAR_ENTER_FONT", "IO_FIXED_CHAR_ENTER_FONT"),
						new ConstantDetails(IFileStructureConstants.IO_TEXT_BYTE_ENTER_FONT, "Text IO (Byte), Enter Font", "TEXT_BYTE_ENTER_FONT", "IO_TEXT_BYTE_ENTER_FONT"),
						new ConstantDetails(IFileStructureConstants.IO_TEXT_CHAR_ENTER_FONT, "Text IO (Char), Enter Font", "TEXT_CHAR_ENTER_FONT", "IO_TEXT_CHAR_ENTER_FONT"),
						new ConstantDetails(IFileStructureConstants.IO_CSV, "Csv Embedded Cr", "CSV_EMBEDDED_CR", "IO_CSV"),
						new ConstantDetails(IFileStructureConstants.IO_BIN_CSV, "", "", "IO_BIN_CSV"),
						new ConstantDetails(IFileStructureConstants.IO_UNICODE_CSV, "Unicode Csv Embedded Cr", "UNICODE_CSV_EMBEDDED_CR", "IO_UNICODE_CSV"),
						new ConstantDetails(IFileStructureConstants.IO_CSV_NAME_1ST_LINE, "Csv Name on 1st line (Embedded Cr)", "CSV_NAME_1ST_LINE_EMBEDDED_CR", "IO_CSV_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_BIN_CSV_NAME_1ST_LINE, "", "", "IO_BIN_CSV_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_UNICODE_CSV_NAME_1ST_LINE, "Unicode Name on 1st line (Embedded Cr)", "UNICODE_CSV_NAME_1ST_LINE_EMBEDDED_CR", "IO_UNICODE_CSV_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_NAME_1ST_LINE, "Csv Name on 1st line", "CSV_NAME_1ST_LINE", "IO_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_GENERIC_CSV, "Generic CSV (Choose details at run time)", "CSV_GENERIC", "IO_GENERIC_CSV"),
//
						new ConstantDetails(IFileStructureConstants.IO_BIN_NAME_1ST_LINE, "Text IO (byte Based) name 1st Line", "Byte_Text_NAME_1ST_LINE", "IO_BIN_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE, "Unicode Name on 1st line", "UNICODE_CSV_NAME_1ST_LINE", "IO_UNICODE_NAME_1ST_LINE"),
						new ConstantDetails(IFileStructureConstants.IO_XML_USE_LAYOUT, "XML - Existing Layout", "XML_Use_Layout", "IO_XML_USE_LAYOUT"),
						new ConstantDetails(IFileStructureConstants.IO_XML_BUILD_LAYOUT, "XML - Build Layout", "XML_Build_Layout", "IO_XML_BUILD_LAYOUT"),
						new ConstantDetails(IFileStructureConstants.IO_STANDARD_UNICODE_TEXT_FILE, "Text IO (Unicode)", "Text_Unicode", "IO_STANDARD_UNICODE_TEXT_FILE")
								.setExtraNames("IO_UNICODE_TEXT"),
	
						new ConstantDetails(IFileStructureConstants.IO_GETTEXT_PO, "GetText PO", "GetText_PO", "IO_GETTEXT_PO"),
						new ConstantDetails(IFileStructureConstants.IO_TIP, "Tip File", "Tip_File", "IO_TIP"),

//						new ConstantDetails(IFileStructureConstants.IO_FIXED_BYTE_ENTER_FONT, "Fixed Byte, enter font", "FIXED_BYTE_ENTER_FONT", "IO_FIXED_BYTE_ENTER_FONT"),
//						new ConstantDetails(IFileStructureConstants.IO_FIXED_CHAR_ENTER_FONT, "Fixed Char, enter font", "FIXED_CHAR_ENTER_FONT", "IO_FIXED_CHAR_ENTER_FONT"),
//						new ConstantDetails(IFileStructureConstants.IO_TEXT_BYTE_ENTER_FONT, "Text IO (Byte), Enter Font", "TEXT_BYTE_ENTER_FONT", "IO_TEXT_BYTE_ENTER_FONT"),
//						new ConstantDetails(IFileStructureConstants.IO_TEXT_CHAR_ENTER_FONT, "Text IO (Char), Enter Font", "TEXT_CHAR_ENTER_FONT", "IO_TEXT_CHAR_ENTER_FONT"),

				});
			}
			return fileStructures;
		}


		/**
		 * @return the typeNames
		 */
		@SuppressWarnings("deprecation")
		public ConstantNameConversion getTypeNames() {
			if (typeNames == null) {
				typeNames = new ConstantNameConversion(Type.class, new ConstantDetails[] {
						  createTypeConstant(Type.ftChar, "Char", "", "ftChar")
						, createTypeConstant(Type.ftNumAnyDecimal, "Number any decimal", "NumAnyDecimal", "ftNumAnyDecimal")
						, createTypeConstant(Type.ftNumOrEmpty, "Number any decimal or Empty", "NumberOrEmpty", "ftNumOrEmpty")
						, createTypeConstant(Type.ftPositiveNumAnyDecimal, "PositiveNumAnyDecimal", "Number (+ve) any decimal", "ftPositiveNumAnyDecimal")
						, createTypeConstant(Type.ftCharRightJust, "Char (right justified)", "", "ftCharRightJust")
						, createTypeConstant(Type.ftCharNullTerminated, "Char Null terminated", "", "ftCharNullTerminated")
						, createTypeConstant(Type.ftCharNullPadded, "Char Null padded", "", "ftCharNullPadded")
						, createTypeConstant(Type.ftCharNoTrim, "Char (no Trim)", "", "ftCharNoTrim")
						, createTypeConstant(Type.ftHex, "Hex Field", "", "ftHex")
						, createTypeConstant(Type.ftNumLeftJustified, "Num (Left Justified)", "", "ftNumLeftJustified")
						, createTypeConstant(Type.ftNumRightJustified, "Num (Right Justified space padded)", "", "ftNumRightJustified")
						, createTypeConstant(Type.ftNumRightJustifiedPN, "Num (Right Justified space padded) +/- sign", "", "ftNumRightJustifiedPN")
						, createTypeConstant(Type.ftNumRightJustCommaDp, "Num (Right Just space padded, \",\" Decimal)", "", "ftNumRightJustCommaDp")
						, createTypeConstant(Type.ftNumRightJustCommaDpPN, "Num (Right Just space padded, \",\" Decimal) +/- sign", "Num (Right Just space padded, \",\" Decimal) +/- sig", "ftNumRightJustCommaDpPN")
						, createTypeConstant(Type.ftNumZeroPadded, "Num (Right Justified zero padded)", "Zero Padded Number with sign=+/-", "ftNumZeroPadded")
						, createTypeConstant(Type.ftNumZeroPaddedPN, "Num (Right Justified zero padded +/- sign)", "Zero Padded Number with sign=+/-", "ftNumZeroPaddedPN")
						, createTypeConstant(Type.ftAssumedDecimal, "Num Assumed Decimal (Zero padded)", "", "ftAssumedDecimal")
						, createTypeConstant(Type.ftAssumedDecimalPositive, "Num Assumed Decimal (+ve)", "", "ftAssumedDecimalPositive")
						, createTypeConstant(Type.ftNumZeroPaddedPositive, "Num (Right Justified zero padded positive)", "Positive Zero Padded Number", "ftNumZeroPaddedPositive")
						, createTypeConstant(Type.ftNumCommaDecimal, "Zero Padded Number decimal=\",\"", "", "ftNumCommaDecimal")
						, createTypeConstant(Type.ftNumCommaDecimalPN, "Zero Padded Number decimal=\",\" sign=+/-", "", "ftNumCommaDecimalPN")
						, createTypeConstant(Type.ftNumCommaDecimalPositive, "Zero Padded Number decimal=\",\" (only +ve)", "", "ftNumCommaDecimalPositive")
						, createTypeConstant(Type.ftSignSeparateLead, "Num Sign Separate Leading", "", "ftSignSeparateLead")
						, createTypeConstant(Type.ftSignSeparateTrail, "Num Sign Separate Trailing", "", "ftSignSeparateTrail")
						, createTypeConstant(Type.ftSignSepLeadActualDecimal, "Num Sign Sep Leading Actual Dec", "", "ftSignSepLeadActualDecimal")
						, createTypeConstant(Type.ftSignSepTrailActualDecimal, "Num Sign Sep Trailing Actual Dec", "", "ftSignSepTrailActualDecimal")
						, createTypeConstant(Type.ftDecimal, "Decimal", "", "ftDecimal")
						, createTypeConstant(Type.ftBinaryInt, "Binary Integer", "", "ftBinaryInt")
						, createTypeConstant(Type.ftBinaryIntPositive, "Binary Integer (only +ve)", "", "ftBinaryIntPositive")
						, createTypeConstant(Type.ftPostiveBinaryInt, "Positive Binary Integer", "Postive Binary Integer", "ftPostiveBinaryInt")
						, createTypeConstant(Type.ftFloat, "Float", "", "ftFloat")
						, createTypeConstant(Type.ftDouble, "Double", "", "ftDouble")
						, createTypeConstant(Type.ftBit, "Bit", "", "ftBit")
						, createTypeConstant(Type.ftPackedDecimal, "Mainframe Packed Decimal (comp-3)", "", "ftPackedDecimal")
						, createTypeConstant(Type.ftPackedDecimalPostive, "Mainframe Packed Decimal (+ve)", "", "ftPackedDecimalPostive")
						, createTypeConstant(Type.ftZonedNumeric, "Mainframe Zoned Numeric", "", "ftZonedNumeric")
						, new ConstantDetails(Type.ftBinaryBigEndian, "Binary Integer Big Endian (Mainframe?)", "Binary Integer Big Endian (Mainframe, AIX etc)", "ftBinaryBigEndian")
								.setExtraNames(
											"Binary Integer Big Endian (Mainframe, AI"
										,	"Binary Integer Big Edian (Mainframe, AIX etc)"
								)
								
						, createTypeConstant(Type.ftComboItemField, "ComboItemField", "ComboItemField" ,"ftComboItemField")
						, createTypeConstant(Type.ftBinaryBigEndian, "Binary Integer Big Endian (Mainframe?)", "Binary Integer Big Endian (Mainframe, AIX etc)", "ftBinaryBigEndian")
						, createTypeConstant(Type.ftBinaryBigEndianPositive, "Binary Integer Big Endian (only +ve)", "Binary Integer Big Endian (only +ve )", "ftBinaryBigEndianPositive")
						, createTypeConstant(Type.ftPositiveBinaryBigEndian, "Positive Integer Big Endian", "Positive Integer (Big Endian)", "ftPositiveBinaryBigEndian")
						, createTypeConstant(Type.ftFjZonedNumeric, "Fujitsu Zoned Numeric", "", "ftFjZonedNumeric")
						, createTypeConstant(Type.ftGnuCblZonedNumeric, "GNU Cobol Zoned Numeric", "", "ftGnuCblZonedNumeric")
						, createTypeConstant(Type.ftRmComp, "Rm Cobol Comp", "", "ftRmComp")
						, createTypeConstant(Type.ftRmCompPositive, "Rm Cobol Comp (+ve)", "RM Cobol Positive Comp", "ftRmCompPositive")
						, createTypeConstant(Type.ftCheckBoxBoolean, "Check Box (Boolean)", "", "ftCheckBoxBoolean")
						, createTypeConstant(Type.ftDate, "Date - Format in Parameter field", "", "ftDate")
						, createTypeConstant(Type.ftDateYMD, "Date - YYMMDD", "", "ftDateYMD")
						, createTypeConstant(Type.ftDateYYMD, "Date - YYYYMMDD", "", "ftDateYYMD")
						, createTypeConstant(Type.ftDateDMY, "Date - DDMMYY", "", "ftDateDMY")
						, createTypeConstant(Type.ftDateDMYY, "Date - DDMMYYYY", "", "ftDateDMYY")
						, createTypeConstant(Type.ftCheckBoxTrue, "Check Box True / Space", "", "ftCheckBoxTrue")
						, createTypeConstant(Type.ftCheckBoxY, "Checkbox Y/<null>", "CheckBox Y/null", "ftCheckBoxY")
						, createTypeConstant(Type.ftCheckBoxYN, "Checkbox Y/N", "", "ftCheckBoxYN")
						, createTypeConstant(Type.ftCheckBoxTF, "Checkbox T/F", "", "ftCheckBoxTF")
						, createTypeConstant(Type.ftCsvArray, "CSV array", "", "ftCsvArray")
						, createTypeConstant(Type.ftXmlNameTag, "XML Name Tag", "", "ftXmlNameTag")
						, createTypeConstant(Type.ftMultiLineEdit, "Edit Multi Line field", "", "ftMultiLineEdit")
						, createTypeConstant(Type.ftCharRestOfFixedRecord, "Char Rest of Fixed Length", "", "ftCharRestOfFixedRecord")
						, createTypeConstant(Type.ftCharRestOfRecord, "Char Rest of Record", "", "ftCharRestOfRecord")
						, createTypeConstant(Type.ftMultiLineChar, "Char (Multi-Line)", "Char Multi Line", "ftMultiLineChar")
						, createTypeConstant(Type.ftCharMultiLine, "Char (Multi-Line) - Old", "", "ftCharMultiLine")
						, createTypeConstant(Type.ftHtmlField, "Html Field", "", "ftHtmlField")
						, createTypeConstant(Type.ftArrayField, "Array Field", "", "ftArrayField")
						, createTypeConstant(Type.ftRecordEditorType, "RecordEditor_Type", "", "ftRecordEditorType")
						, createTypeConstant(Type.ftPackedDecimalSmall, "Small_PackedDecimal", "", "ftPackedDecimalSmall")
						, createTypeConstant(Type.ftPackedDecimalSmallPostive, "Small_Positive_Packed_Decimal", "", "ftPackedDecimalSmallPostive")
						, createTypeConstant(Type.ftIntBigEndianSmall, "Small_Big_Endian", "", "ftIntBigEndianSmall")
						, createTypeConstant(Type.ftIntBigEndianPositive, "Small ", "", "ftIntBigEndianPositive")
						, createTypeConstant(Type.ftUIntBigEndianSmall, "Small_UInt_Big_Endian", "", "ftUIntBigEndianSmall")
						, createTypeConstant(Type.ftIntSmall, "Small_Int", "", "ftIntSmall")
						, createTypeConstant(Type.ftIntPositiveSmall, "Small_Positive_Int", "", "ftIntPositiveSmall")
						, createTypeConstant(Type.ftUIntSmall, "Small_Unsigned_Int", "", "ftUIntSmall")
						, createTypeConstant(Type.ftZonedEbcdicSmall, "Small_Zoned_EBCDIC", "", "ftZonedEbcdicSmall")
						, createTypeConstant(Type.ftZonedEbcdicSmallPositive, "Small_Positive_Zoned_EBCDIC", "", "ftZonedEbcdicSmallPositive")
						, createTypeConstant(Type.ftZonedAsciiSmall, "Small_Zoned_Ascii", "", "ftZonedAsciiSmall")
						, createTypeConstant(Type.ftZonedAsciiSmallPositive, "Small_Positive_Zoned_Ascii", "", "ftZonedAsciiSmallPositive")						
				});
			}
			return typeNames;
		}


		/**
		 * @return the dialects
		 */
		public ConstantNameConversion getDialects() {
			if (dialects == null) {
				dialects = new ConstantNameConversion(ICopybookDialects.class, new ConstantDetails[] {
						createDialectConstant(ICopybookDialects.FMT_INTEL, ICopybookDialects.FMT_INTEL_NAME, "FMT_INTEL"),
						createDialectConstant(ICopybookDialects.FMT_MAINFRAME, ICopybookDialects.FMT_MAINFRAME_NAME, "FMT_MAINFRAME"),
						createDialectConstant(ICopybookDialects.FMT_FUJITSU, ICopybookDialects.FMT_FUJITSU_NAME, "FMT_FUJITSU"),
						createDialectConstant(ICopybookDialects.FMT_BIG_ENDIAN, ICopybookDialects.FMT_BIG_ENDIAN_NAME, "FMT_BIG_ENDIAN"),
						createDialectConstant(ICopybookDialects.FMT_GNU_COBOL, ICopybookDialects.FMT_GNU_COBOL_NAME, "FMT_GNU_COBOL"),
						
						createDialectConstant(ICopybookDialects.FMT_FS2000, ICopybookDialects.FMT_FS2000_NAME, "FMT_FS2000"),
						createDialectConstant(ICopybookDialects.FMT_GNU_COBOL_MVS, ICopybookDialects.FMT_GNU_COBOL_MVS_NAME, "FMT_GNU_COBOL_MVS"),
						createDialectConstant(ICopybookDialects.FMT_GNU_COBOL_MF, ICopybookDialects.FMT_GNU_COBOL_MF_NAME, "FMT_GNU_COBOL_MF"),
						createDialectConstant(ICopybookDialects.FMT_GNU_COBOL_BE, ICopybookDialects.FMT_GNU_COBOL_BE_NAME, "FMT_GNU_COBOL_BE"),
						createDialectConstant(ICopybookDialects.FMT_FS2000_BE, ICopybookDialects.FMT_FS2000_BE_NAME, "FMT_FS2000_BE"),
						
						createDialectConstant(ICopybookDialects.FMT_GNU_COBOL_MVS_BE, ICopybookDialects.FMT_GNU_COBOL_MVS_BE_NAME, "FMT_GNU_COBOL_MVS_BE"),
						createDialectConstant(ICopybookDialects.FMT_OC_MICRO_FOCUS_BE, ICopybookDialects.FMT_OC_MICRO_FOCUS_BE_NAME, "FMT_OC_MICRO_FOCUS_BE"),
//						createDialectConstant(ICopybookDialects.FMT_MICRO_FOCUS, ICopybookDialects.FMT_MICRO_FOCUS_NAME, "FMT_MICRO_FOCUS"),
						createDialectConstant(ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL, ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL_NAME, "FMT_MAINFRAME_COMMA_DECIMAL"),
						createDialectConstant(ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL, ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL_NAME, "FMT_FUJITSU_COMMA_DECIMAL"),				});
			}
			return dialects;
		}
		
		public ConstantNameConversion getSplitOptions() {
			if (splitOptions == null) {
				splitOptions = new ConstantNameConversion(ICobolSplitOptions.class, new ConstantDetails[] {
						createSplitOption(ICobolSplitOptions.SPLIT_NONE, "SPLIT_NONE", "NONE"),
						createSplitOption(ICobolSplitOptions.SPLIT_01_LEVEL, "SPLIT_01_LEVEL", "01"),
						createSplitOption(ICobolSplitOptions.SPLIT_HIGHEST_REPEATING, "SPLIT_HIGHEST_REPEATING", "HIGHEST_REPEATING"),
						createSplitOption(ICobolSplitOptions.SPLIT_REDEFINE, "SPLIT_REDEFINE", "REDEFINE"),
						createSplitOption(ICobolSplitOptions.SPLIT_TOP_LEVEL, "SPLIT_TOP_LEVEL", "TOP_LEVEL"),
				});
			}
			return splitOptions;
		}
		
		/**
		 * @return the recordTypes
		 */
		public ConstantNameConversion getRecordTypes() {
			if (recordTypes == null) {
				recordTypes = new ConstantNameConversion("Constants", "net.sf.JRecord.Common.Constants", new ConstantDetails[] {
						createRecordType(Constants.rtBinaryRecord, "BinaryRecord", "rtBinaryRecord"),
						createRecordType(Constants.rtRecordLayout, "RecordLayout", "rtRecordLayout"),
						createRecordType(Constants.rtDelimited, "Delimited", "rtDelimited"),
						createRecordType(Constants.rtDelimitedAndQuote, "DelimitedAndQuote", "rtDelimitedAndQuote"),
						createRecordType(Constants.RT_XML, "XML", "RT_XML"),
						createRecordType(Constants.rtGroupOfRecords, "GroupOfRecords", "rtGroupOfRecords"),
						createRecordType(Constants.rtGroupOfBinaryRecords, "GroupOfBinaryRecords", "rtGroupOfBinaryRecords"),
						createRecordType(Constants.rtFixedLengthRecords, "FixedLengthRecords", "rtFixedLengthRecords"),
						});
			}
			return recordTypes;
		}
		
		public ConstantNameConversion getCopybookFormat() {
			if (copybookFormats == null) {
				copybookFormats =  new ConstantNameConversion("Cb2xmlConstants", "net.sf.cb2xml.def.Cb2xmlConstants", new ConstantDetails[] {
						createCopybookFormatOption(Cb2xmlConstants.USE_STANDARD_COLUMNS, "USE_STANDARD_COLUMNS"),
						createCopybookFormatOption(Cb2xmlConstants.USE_SUPPLIED_COLUMNS, "USE_SUPPLIED_COLUMNS"),
						createCopybookFormatOption(Cb2xmlConstants.USE_COLS_6_TO_80    , "USE_COLS_6_TO_80"),                           
						createCopybookFormatOption(Cb2xmlConstants.USE_LONG_LINE       , "USE_LONG_LINE"),
						createCopybookFormatOption(Cb2xmlConstants.USE_PROPERTIES_FILE , "USE_PROPERTIES_FILE"),
						createCopybookFormatOption(Cb2xmlConstants.FREE_FORMAT         , "FREE_FORMAT"),  
				});
			};
			
			return copybookFormats;
		}

			


		private  static ConstantDetails createCopybookFormatOption(int code,String name) {
			String externalName = name.startsWith("USE_")  ? name.substring(4) : name;
			return new ConstantDetails(code, name, externalName, name);
		}

		private  static ConstantDetails createSplitOption(int code, String name, String altName) {
			return new ConstantDetails(code, altName, name, name);
		}

		
		private static ConstantDetails createTypeConstant(int code, String simpleName, String externalName, String jrecordConstant) {
			if (len(simpleName) > 40 || len(externalName) > 40) {
				return new ConstantTypeDetails(code, simpleName, externalName, jrecordConstant);
			}
			return new ConstantDetails(code, simpleName, externalName, jrecordConstant);
		}
		
		private static ConstantDetails createDialectConstant(int code, String simpleName, String jrecordConstant) {
			String externalName = Conversion.replace(simpleName, " ", "_").toString();
			return new ConstantTypeDetails(code, simpleName, externalName, jrecordConstant);
		}
		
		private  static ConstantDetails createRecordType(int code, String name, String jrecordName) {
			return new ConstantDetails(code, name, name, jrecordName);
		}

	}
	
	private static int len(String s) {
		return s == null ? 0 : s.length();
	}
	
	private static class ConstantTypeDetails extends ConstantDetails {

		public ConstantTypeDetails(int code, String simpleName, String externalName, String jrecordConstant) {
			super(code, simpleName, externalName, jrecordConstant);
		}

		@Override String[] getExtraNames() {
			String simpleName = super.getSimpleName();
			String externalName = super.getExternalName();
			boolean truncSimpleName = len(simpleName) > 40;
			boolean truncExternalName = len(externalName) > 40;
			
			if (truncSimpleName && truncExternalName) {
				return new String[] {simpleName.substring(0, 40), externalName.substring(0, 40)};
			}
			if (truncExternalName) {
				return new String[] {externalName.substring(0, 40)};
			}
		
			if (truncSimpleName) {
				return new String[] {simpleName.substring(0, 40)};
			}
			
			return super.getExtraNames();
		}
		
	}
}
