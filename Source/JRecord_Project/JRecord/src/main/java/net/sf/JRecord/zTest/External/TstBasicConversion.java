package net.sf.JRecord.zTest.External;

import static org.junit.Assert.*;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.BasicConversion;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;

public class TstBasicConversion {

//	@Test
//	public void test() {
//		String[] typeNames = getTypeConstantNames();
//		BasicConversion conv = new BasicConversion();
//		
//		for (int i = 0; i < 140; i++) {
//			if (typeNames[i] != null) {
//				String typeAsString = conv.getTypeAsString(0, i);
//				if (typeAsString.length() > 2) {
//					System.out.println("\ttypeName[Type." + typeNames[i] + "] = \"" + typeAsString + "\";");
//				}
//			}
//		}
//	}
//	
	@Test
	public void testTypeStringConversion() {
		String[] typeNames = getTypeNames();
		BasicConversion conv = new BasicConversion();
		
		for (int i = 0; i < typeNames.length; i++) {
			if (typeNames[i] != null) {
				assertEquals(typeNames[i], conv.getTypeAsString(0, i));
			}
		}
		
		tstTypeNames(typeNames, conv);
		tstTypeNames(getTypeConstantNames(), conv);
	}



	private void tstTypeNames(String[] typeNames, BasicConversion conv) {
		for (int i = 0; i < typeNames.length; i++) {
			if (typeNames[i] != null) {
				assertEquals(typeNames[i], i, conv.getType(0, typeNames[i]));
				assertEquals(i, conv.getType(0, typeNames[i].toLowerCase()));
				assertEquals(i, conv.getType(0, typeNames[i].toUpperCase()));
			}
		}
	}
	
	@Test
	public void testDialects() {
		String[] dialects = getDialectNames();
		BasicConversion conv = new BasicConversion();
		
		for (int i = 0; i < dialects.length; i++) {
			if (dialects[i] != null) {
				String dialect = Conversion.replace(dialects[i], " ", "_").toString();
				assertEquals("" + i, dialect, conv.getDialectName(i));
				assertEquals(i, conv.getDialect(dialect));
				assertEquals(i, conv.getDialect( dialects[i]));
				assertEquals(i, conv.getDialect("" + i));
			}
		}
	}

	private static String[] getDialectNames() {
		String[] dialects = new String[50];
		dialects[ICopybookDialects.FMT_INTEL] = ICopybookDialects.FMT_INTEL_NAME;
		dialects[ICopybookDialects.FMT_MAINFRAME] = ICopybookDialects.FMT_MAINFRAME_NAME;
		dialects[ICopybookDialects.FMT_FUJITSU] = ICopybookDialects.FMT_FUJITSU_NAME;
		dialects[ICopybookDialects.FMT_BIG_ENDIAN] = ICopybookDialects.FMT_BIG_ENDIAN_NAME;
		dialects[ICopybookDialects.FMT_GNU_COBOL] = ICopybookDialects.FMT_GNU_COBOL_NAME;
		dialects[ICopybookDialects.FMT_FS2000] = ICopybookDialects.FMT_FS2000_NAME;
		dialects[ICopybookDialects.FMT_GNU_COBOL_MVS] = ICopybookDialects.FMT_GNU_COBOL_MVS_NAME;
		dialects[ICopybookDialects.FMT_GNU_COBOL_MF] = ICopybookDialects.FMT_GNU_COBOL_MF_NAME;
		dialects[ICopybookDialects.FMT_GNU_COBOL_BE] = ICopybookDialects.FMT_GNU_COBOL_BE_NAME;
		dialects[ICopybookDialects.FMT_FS2000_BE] = ICopybookDialects.FMT_FS2000_BE_NAME;
		dialects[ICopybookDialects.FMT_GNU_COBOL_MVS_BE] = ICopybookDialects.FMT_GNU_COBOL_MVS_BE_NAME;
		dialects[ICopybookDialects.FMT_OC_MICRO_FOCUS_BE] = ICopybookDialects.FMT_OC_MICRO_FOCUS_BE_NAME;
//		dialects[ICopybookDialects.FMT_MICRO_FOCUS] = ICopybookDialects.FMT_MICRO_FOCUS_NAME;
		dialects[ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL] = ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL_NAME;
		dialects[ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL] = ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL_NAME;
		
		return dialects;

	}

	
	@SuppressWarnings("deprecation")
	private static String[] getTypeNames() {
		String[] typeName = new String[250];
		
		typeName[Type.ftChar] = "Char";
		typeName[Type.ftCharRightJust] = "Char (right justified)";
		typeName[Type.ftCharNullTerminated] = "Char Null terminated";
		typeName[Type.ftCharNullPadded] = "Char Null padded";
		typeName[Type.ftHex] = "Hex Field";
		typeName[Type.ftNumLeftJustified] = "Num (Left Justified)";
		typeName[Type.ftNumRightJustified] = "Num (Right Justified space padded)";
		typeName[Type.ftNumZeroPadded] = "Num (Right Justified zero padded)";
		typeName[Type.ftAssumedDecimal] = "Num Assumed Decimal (Zero padded)";
		typeName[Type.ftSignSeparateLead] = "Num Sign Separate Leading";
		typeName[Type.ftSignSeparateTrail] = "Num Sign Separate Trailing";
		typeName[Type.ftDecimal] = "Decimal";
		typeName[Type.ftBinaryInt] = "Binary Integer";
		typeName[Type.ftPostiveBinaryInt] = "Positive Binary Integer";
		typeName[Type.ftFloat] = "Float";
		typeName[Type.ftDouble] = "Double";
		typeName[Type.ftNumAnyDecimal] = "Number any decimal";
		typeName[Type.ftPositiveNumAnyDecimal] = "PositiveNumAnyDecimal";
		typeName[Type.ftBit] = "Bit";
		typeName[Type.ftAssumedDecimalPositive] = "Num Assumed Decimal (+ve)";
		typeName[Type.ftBinaryIntPositive] = "Binary Integer (only +ve)";
		typeName[Type.ftNumZeroPaddedPN] = "Num (Right Justified zero padded +/- sign)";
		typeName[Type.ftNumZeroPaddedPositive] = "Num (Right Justified zero padded positive)";
		typeName[Type.ftNumCommaDecimal] = "Zero Padded Number decimal=\",\"";
		typeName[Type.ftNumCommaDecimalPN] = "Zero Padded Number decimal=\",\" sign=+/-";
		typeName[Type.ftNumCommaDecimalPositive] = "Zero Padded Number decimal=\",\" (only +ve)";
		typeName[Type.ftNumRightJustifiedPN] = "Num (Right Justified space padded) +/- sign";
		typeName[Type.ftPackedDecimal] = "Mainframe Packed Decimal (comp-3)";
		typeName[Type.ftZonedNumeric] = "Mainframe Zoned Numeric";
		typeName[Type.ftPackedDecimalPostive] = "Mainframe Packed Decimal (+ve)";
		typeName[Type.ftBinaryBigEndian] = "Binary Integer Big Endian (Mainframe?)";
		typeName[Type.ftPositiveBinaryBigEndian] = "Positive Integer Big Endian";
		typeName[Type.ftRmComp] = "Rm Cobol Comp";
		typeName[Type.ftRmCompPositive] = "Rm Cobol Comp (+ve)";
		typeName[Type.ftBinaryBigEndianPositive] = "Binary Integer Big Endian (only +ve)";
		typeName[Type.ftFjZonedNumeric] = "Fujitsu Zoned Numeric";
		typeName[Type.ftNumRightJustCommaDp] = "Num (Right Just space padded, \",\" Decimal)";
		typeName[Type.ftNumRightJustCommaDpPN] = "Num (Right Just space padded, \",\" Decimal) +/- sign";
		typeName[Type.ftSignSepLeadActualDecimal] = "Num Sign Sep Leading Actual Dec";
		typeName[Type.ftSignSepTrailActualDecimal] = "Num Sign Sep Trailing Actual Dec";
		typeName[Type.ftGnuCblZonedNumeric] = "GNU Cobol Zoned Numeric";
		typeName[Type.ftCharMultiLine] = "Char (Multi-Line) - Old";
		typeName[Type.ftDate] = "Date - Format in Parameter field";
		typeName[Type.ftDateYMD] = "Date - YYMMDD";
		typeName[Type.ftDateYYMD] = "Date - YYYYMMDD";
		typeName[Type.ftDateDMY] = "Date - DDMMYY";
		typeName[Type.ftDateDMYY] = "Date - DDMMYYYY";
		typeName[Type.ftCharRestOfFixedRecord] = "Char Rest of Fixed Length";
		typeName[Type.ftCharRestOfRecord] = "Char Rest of Record";
		typeName[Type.ftCharNoTrim] = "Char (no Trim)";
		typeName[Type.ftArrayField] = "Array Field";
		typeName[Type.ftCheckBoxY] = "Checkbox Y/<null>";
		typeName[Type.ftCheckBoxTrue] = "Check Box True / Space";
		typeName[Type.ftCheckBoxYN] = "Checkbox Y/N";
		typeName[Type.ftCheckBoxTF] = "Checkbox T/F";
		typeName[Type.ftCheckBoxBoolean] = "Check Box (Boolean)";
		typeName[Type.ftCsvArray] = "CSV array";
		typeName[Type.ftXmlNameTag] = "XML Name Tag";
		typeName[Type.ftMultiLineEdit] = "Edit Multi Line field";
		typeName[Type.ftMultiLineChar] = "Char (Multi-Line)";
		typeName[Type.ftHtmlField] = "Html Field";
		typeName[Type.ftRecordEditorType] = "RecordEditor_Type";
		typeName[Type.ftNumOrEmpty] = "Number any decimal or Empty";
		
		return typeName;
	}

	
	@SuppressWarnings("deprecation")
	private static String[] getTypeConstantNames() {
		String[] typeName = new String[250];
		typeName[Type.ftChar] = "ftChar";
		typeName[Type.ftCharRightJust] = "ftCharRightJust";
		typeName[Type.ftCharNullTerminated] = "ftCharNullTerminated";
		typeName[Type.ftCharNullPadded] = "ftCharNullPadded";
		typeName[Type.ftHex] = "ftHex";
		typeName[Type.ftNumLeftJustified] = "ftNumLeftJustified";
		typeName[Type.ftNumRightJustified] = "ftNumRightJustified";
		typeName[Type.ftNumZeroPadded] = "ftNumZeroPadded";
		typeName[Type.ftAssumedDecimal] = "ftAssumedDecimal";
		typeName[Type.ftSignSeparateLead] = "ftSignSeparateLead";
		typeName[Type.ftSignSeparateTrail] = "ftSignSeparateTrail";
		typeName[Type.ftSignSepLeadActualDecimal] = "ftSignSepLeadActualDecimal";
		typeName[Type.ftSignSepTrailActualDecimal] = "ftSignSepTrailActualDecimal";
		typeName[Type.ftDecimal] = "ftDecimal";
		typeName[Type.ftBinaryInt] = "ftBinaryInt";
		typeName[Type.ftPostiveBinaryInt] = "ftPostiveBinaryInt";
		typeName[Type.ftFloat] = "ftFloat";
		typeName[Type.ftDouble] = "ftDouble";
		typeName[Type.ftNumAnyDecimal] = "ftNumAnyDecimal";
		typeName[Type.ftPositiveNumAnyDecimal] = "ftPositiveNumAnyDecimal";
		typeName[Type.ftBit] = "ftBit";
		typeName[Type.ftAssumedDecimalPositive] = "ftAssumedDecimalPositive";
		typeName[Type.ftBinaryIntPositive] = "ftBinaryIntPositive";
		typeName[Type.ftNumZeroPaddedPN] = "ftNumZeroPaddedPN";
		typeName[Type.ftNumZeroPaddedPositive] = "ftNumZeroPaddedPositive";
		typeName[Type.ftNumCommaDecimal] = "ftNumCommaDecimal";
		typeName[Type.ftNumCommaDecimalPN] = "ftNumCommaDecimalPN";
		typeName[Type.ftNumCommaDecimalPositive] = "ftNumCommaDecimalPositive";
		typeName[Type.ftNumRightJustifiedPN] = "ftNumRightJustifiedPN";
		typeName[Type.ftPackedDecimal] = "ftPackedDecimal";
		typeName[Type.ftZonedNumeric] = "ftZonedNumeric";
		typeName[Type.ftPackedDecimalPostive] = "ftPackedDecimalPostive";
		typeName[Type.ftBinaryBigEndian] = "ftBinaryBigEndian";
		typeName[Type.ftBinaryBigEndianPositive] = "ftBinaryBigEndianPositive";
		typeName[Type.ftPositiveBinaryBigEndian] = "ftPositiveBinaryBigEndian";
		typeName[Type.ftRmComp] = "ftRmComp";
		typeName[Type.ftRmCompPositive] = "ftRmCompPositive";
		typeName[Type.ftFjZonedNumeric] = "ftFjZonedNumeric";
		typeName[Type.ftNumRightJustCommaDp] = "ftNumRightJustCommaDp";
		typeName[Type.ftNumRightJustCommaDpPN] = "ftNumRightJustCommaDpPN";
		typeName[Type.ftGnuCblZonedNumeric] = "ftGnuCblZonedNumeric";
		typeName[Type.ftCharMultiLine] = "ftCharMultiLine";
		typeName[Type.ftDate] = "ftDate";
		typeName[Type.ftDateYMD] = "ftDateYMD";
		typeName[Type.ftDateYYMD] = "ftDateYYMD";
		typeName[Type.ftDateDMY] = "ftDateDMY";
		typeName[Type.ftDateDMYY] = "ftDateDMYY";
		typeName[Type.ftCharRestOfFixedRecord] = "ftCharRestOfFixedRecord";
		typeName[Type.ftCharRestOfRecord] = "ftCharRestOfRecord";
		typeName[Type.ftCharNoTrim] = "ftCharNoTrim";
//		typeName[Type.ftProtoField] = "ftProtoField";
//		typeName[Type.ftAvroField] = "ftAvroField";
		typeName[Type.ftArrayField] = "ftArrayField";
//		typeName[Type.ftComboItemField] = "ftComboItemField";
//		typeName[Type.ftAvroUnionField] = "ftAvroUnionField";
		typeName[Type.ftCheckBoxY] = "ftCheckBoxY";
		typeName[Type.ftCheckBoxTrue] = "ftCheckBoxTrue";
		typeName[Type.ftCheckBoxYN] = "ftCheckBoxYN";
		typeName[Type.ftCheckBoxTF] = "ftCheckBoxTF";
		typeName[Type.ftCheckBoxBoolean] = "ftCheckBoxBoolean";
		typeName[Type.ftCsvArray] = "ftCsvArray";
		typeName[Type.ftXmlNameTag] = "ftXmlNameTag";
		typeName[Type.ftMultiLineEdit] = "ftMultiLineEdit";
		typeName[Type.ftMultiLineChar] = "ftMultiLineChar";
		typeName[Type.ftHtmlField] = "ftHtmlField";
		typeName[Type.ftRecordEditorType] = "ftRecordEditorType";
		typeName[Type.ftNumOrEmpty] = "ftNumOrEmpty";
		typeName[Type.ftPackedDecimalSmall] = "ftPackedDecimalSmall";
		typeName[Type.ftPackedDecimalSmallPostive] = "ftPackedDecimalSmallPostive";
		typeName[Type.ftIntBigEndianSmall] = "ftIntBigEndianSmall";
		typeName[Type.ftIntBigEndianPositive] = "ftIntBigEndianPositive";
		typeName[Type.ftUIntBigEndianSmall] = "ftUIntBigEndianSmall";
		typeName[Type.ftIntSmall] = "ftIntSmall";
		typeName[Type.ftIntPositiveSmall] = "ftIntPositiveSmall";
		typeName[Type.ftUIntSmall] = "ftUIntSmall";
		typeName[Type.ftZonedEbcdicSmall] = "ftZonedEbcdicSmall";
		typeName[Type.ftZonedEbcdicSmallPositive] = "ftZonedEbcdicSmallPositive";
		typeName[Type.ftZonedAsciiSmall] = "ftZonedAsciiSmall";
		typeName[Type.ftZonedAsciiSmallPositive] = "ftZonedAsciiSmallPositive";

		return typeName;
	}
}
