package net.sf.JRecord.zTest.External;

import static org.junit.Assert.*;

import org.junit.Test;

import net.sf.JRecord.External.Def.BasicConversion;
import net.sf.JRecord.External.base.ExternalConversion;

public class TstExternalConversion1 {

	private static final String[] STANDARD_NAMES = {
	          "Default",
              "Text",
              "Fixed Length Binary",
              "Line based Binary",
              "Mainframe VB (rdw based) Binary",
              "Mainframe VB Dump: includes Block length",
              "Fujitsu Cobol VB",
              "GNU Cobol VB"
	};
	ConversionNames[] STRUCTURE_DETAILS = {
			new ConversionNames(0, "Default", "Default"),
			new ConversionNames(1, "Text IO", "Text"),
			new ConversionNames(9, "Text IO (byte Based)", "Byte_Text"),
			new ConversionNames(90, "Text IO (Unicode)", "Text_Unicode"),
			new ConversionNames(2, "Fixed Length Binary", "Fixed_Length"),
			new ConversionNames(10, "Fixed Length Char", "Fixed_Length_Char"),
			new ConversionNames(3, "Line based Binary", "Binary"),
			new ConversionNames(4, "Mainframe VB (rdw based) Binary", "Mainframe_VB"),
			new ConversionNames(5, "Mainframe VB Dump: includes Block length", "Mainframe_VB_As_RECFMU"),
			new ConversionNames(14, "VB_DUMP2", "VB_DUMP2"),
			new ConversionNames(7, "Fujitsu Variable Binary", "FUJITSU_VB"),
			new ConversionNames(8, "GNU Cobol VB", "Gnu_Cobol_VB"),
			new ConversionNames(31, "Experimental Microfocus Header File", "Microfocus_Format"),
			new ConversionNames(21, "Unknown File Format", "UNKOWN_FORMAT"),
			new ConversionNames(22, "File Wizard", "FILE_WIZARD"),
			new ConversionNames(44, "Csv Embedded Cr", "CSV_EMBEDDED_CR"),
			new ConversionNames(46, "Unicode Csv Embedded Cr", "UNICODE_CSV_EMBEDDED_CR"),
			new ConversionNames(51, "Csv Name on 1st line", "CSV_NAME_1ST_LINE"),
			new ConversionNames(47, "Csv Name on 1st line (Embedded Cr)", "CSV_NAME_1ST_LINE_EMBEDDED_CR"),
			new ConversionNames(54, "Text IO (byte Based) name 1st Line", "Byte_Text_NAME_1ST_LINE"),
			new ConversionNames(55, "Unicode Name on 1st line", "UNICODE_CSV_NAME_1ST_LINE"),
			new ConversionNames(49, "Unicode Name on 1st line (Embedded Cr)", "UNICODE_CSV_NAME_1ST_LINE_EMBEDDED_CR"),
			new ConversionNames(55, "Unicode Name on 1st line", "UNICODE_CSV_NAME_1ST_LINE"),
			new ConversionNames(52, "Generic CSV (Choose details at run time)", "CSV_GENERIC"),
			new ConversionNames(61, "XML - Existing Layout", "XML_Use_Layout"),
			new ConversionNames(62, "XML - Build Layout", "XML_Build_Layout"),
			new ConversionNames(11, "Continuous no eol marker", "Continuous"),
			new ConversionNames(12, "Variable Block Spanned (VBS)", "Mainframe_VBS"),
			new ConversionNames(35, "Fixed Byte, enter font", "FIXED_BYTE_ENTER_FONT"),
			new ConversionNames(36, "Fixed Char, enter font", "FIXED_CHAR_ENTER_FONT"),
			new ConversionNames(37, "Text IO (Byte), Enter Font", "TEXT_BYTE_ENTER_FONT"),
			new ConversionNames(38, "Text IO (Char), Enter Font", "TEXT_CHAR_ENTER_FONT"),
	};
	
	@Test
	public void testStandardNames() {
		for (String s : STANDARD_NAMES) {
			System.out.println(ExternalConversion.getFileStructure(0, s));
		}
		//BasicConversion.getFileStructureForIndex(0)
		
		for (int i = 0; i < BasicConversion.getNumberOfFileStructures(); i++) {
//			System.out.println(BasicConversion.getFileStructureForIndex(i)
//					+ "\t" + BasicConversion.getFileStructureNameForIndex(i)
//					+ "\t" + BasicConversion.getStructureNameForIndex(i));
			
//			System.out.println(
//					"\t\tnew ConversionNames("
//					+ BasicConversion.getFileStructureForIndex(i)
//					+ ", \"" + BasicConversion.getFileStructureNameForIndex(i)
//					+ "\", \"" + BasicConversion.getStructureNameForIndex(i)
//					+ "\"),");
		}
		//fail("Not yet implemented");
	}
	
	@Test
	public void testBasicConversionGetStructureName() {
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].externalName,BasicConversion.getStructureName(STRUCTURE_DETAILS[i].code));
		}
	}

	
	@Test
	public void testBasicConversionGetStructureCodeFromName() {
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].code, BasicConversion.getStructure(STRUCTURE_DETAILS[i].externalName));
		}
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].code, BasicConversion.getStructure(STRUCTURE_DETAILS[i].internalName));
		}
	}
	
	@Test
	public void testGetStructureCodeFromName() {
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].code, ExternalConversion.getFileStructure(0, STRUCTURE_DETAILS[i].externalName));
		}
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].code, ExternalConversion.getFileStructure(0, STRUCTURE_DETAILS[i].internalName));
		}
	}
	
	@Test
	public void testGetStructureName() {
		for (int i = 0; i < STRUCTURE_DETAILS.length; i++) {
			assertEquals(i + " " + STRUCTURE_DETAILS[i].code + " ", 
					STRUCTURE_DETAILS[i].externalName, ExternalConversion.getFileStructureAsString(0, STRUCTURE_DETAILS[i].code));
		}
	}

	private static class ConversionNames {
		final int code;
		final String internalName, externalName;
		
		public ConversionNames(int code, String internalName, String externalName) {
			super();
			this.code = code;
			this.internalName = internalName;
			this.externalName = externalName;
		}
		
	}
}
