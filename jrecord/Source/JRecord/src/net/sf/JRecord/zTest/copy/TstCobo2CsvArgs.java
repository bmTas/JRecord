package net.sf.JRecord.zTest.copy;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalConversion;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zExamples.cobol.toCsv.ParseArgsCobol2Csv;
import junit.framework.TestCase;


/**
 * Check Program arguments are parsed correctly for Cobol2Csv & Csv2Cobol programs
 * 
 * @author Bruce Martin
 *
 */
public class TstCobo2CsvArgs extends TestCase {

	private static final String OUT_FONT = "font2";
	private static final String OUT_FILE = "outFile";
	private static final String IN_FONT = "font1";
	private static final String COPYBOOK = "Copybook";
	private static final String IN_FILE = "inFile";

	public void testParseArgsCobol2Csv() {
		String[] parms1 = {
				ParseArgsCobol2Csv.ARG_IN_FILE,  IN_FILE, 
				ParseArgsCobol2Csv.ARG_COPYBOOK,  COPYBOOK,
				ParseArgsCobol2Csv.ARG_INPUT_FONT, IN_FONT,
				ParseArgsCobol2Csv.ARG_BINARY, "1",
				ParseArgsCobol2Csv.ARG_STRUCTURE, "1",
				ParseArgsCobol2Csv.ARG_OUT_FILE, OUT_FILE,
				ParseArgsCobol2Csv.ARG_OUTPUT_FONT, OUT_FONT,
				ParseArgsCobol2Csv.ARG_CSV_PARSER, "1",
				ParseArgsCobol2Csv.ARG_RENAME, "1",
		};
		String[] parms2= {
				ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE, 
				ParseArgsCobol2Csv.ARG_COPYBOOK1,  COPYBOOK,
				ParseArgsCobol2Csv.ARG_INPUT_FONT1, IN_FONT,
				ParseArgsCobol2Csv.ARG_BINARY1, "1",
				ParseArgsCobol2Csv.ARG_STRUCTURE1, "1",
				ParseArgsCobol2Csv.ARG_OUT_FILE1, OUT_FILE,
				ParseArgsCobol2Csv.ARG_OUTPUT_FONT1, OUT_FONT,
				ParseArgsCobol2Csv.ARG_CSV_PARSER, "1",
				ParseArgsCobol2Csv.ARG_RENAME, "1",
		};
		
		doCheckAllArgs(new ParseArgsCobol2Csv(parms1));
		doCheckAllArgs(new ParseArgsCobol2Csv(parms2));
	}
	
	private void doCheckAllArgs(ParseArgsCobol2Csv a) {
		assertEquals(IN_FILE, a.infile);
		assertEquals(COPYBOOK, a.copybookName);
		assertEquals(IN_FONT, a.inFont);
		assertEquals(1, a.binFormat);
		assertEquals(1, a.fileStructure);
		assertEquals(OUT_FILE, a.outfile);
		assertEquals(OUT_FONT, a.outFont);
		assertEquals(1, a.csvParser);
		assertEquals(1, a.renameOption);
	}
	

	public void testRenameArg() {
		
		ParseArgsCobol2Csv.Option[] options = ParseArgsCobol2Csv.getRenameOptions();
		for (ParseArgsCobol2Csv.Option o : options) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_RENAME, o.display
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(o.code, ac.renameOption);
		}
	}
	

	public void testCsvParserArg() {
		
		ParseArgsCobol2Csv.Option[] options = ParseArgsCobol2Csv.getCsvParserOptions();
		for (ParseArgsCobol2Csv.Option o : options) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_CSV_PARSER, o.display
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(o.code, ac.csvParser);
		}
	}

	

	public void testFileStructure() {
		
		int[] structures = {
				Constants.IO_DEFAULT, Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_CHAR,
				Constants.IO_BIN_TEXT, Constants.IO_TEXT_LINE, Constants.IO_UNICODE_TEXT,
				Constants.IO_VB, Constants.IO_VB_FUJITSU, Constants.IO_VB_OPEN_COBOL,
		};
		for (int fileOrganisation : structures) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_STRUCTURE1, ExternalConversion.getFileStructureAsString(0, fileOrganisation)
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(fileOrganisation, ac.fileStructure);
		}
	}


	public void testDialect() {
		
		int[] dialects = {
				Convert.FMT_MAINFRAME,
				Convert.FMT_FUJITSU,
				Convert.FMT_OPEN_COBOL,
				Convert.FMT_OC_MICRO_FOCUS,
				Convert.FMT_FS2000,
		};
		for (int dialect : dialects) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_BINARY1, ConversionManager.getInstance().getConverter4code(dialect).getName()
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(dialect, ac.binFormat);
		}
	}

}
