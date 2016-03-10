package net.sf.JRecord.zTest.cobol2csv;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2Csv;
import net.sf.JRecord.zExamples.cobol.toCsv.Csv2Cobol;
import net.sf.JRecord.zExamples.cobol.toCsv.ParseArgsCobol2Csv;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;


/**
 * Testing Copying to/from Csv files with embedded field-delimiters and Quote characters
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolToCsv03 extends TestCase {

	static final String[] DELIMS = {
		"|", ",", "\t", "!", ";", "bar", "tab"
	};
	
	static final String[] CONV = {
		"Asis", "no-", "_", "Leave_Asis", "Change_Minus_To_Underscore", "Drop_Minus"
	};
	
	static final String[] QUOTES = {
		"\"", "'", "`"
	};
	
	
	static ICobolIOBuilder iobCbl = JRecordInterface1.COBOL
			.newIOBuilder(new StringReader(Cbl2CsvCommonCode.COBOL_COPBOOK2), "Rec");
	
	public void testCsv01() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q);
				}
			}
		}
	};

	public void tstCsv(String delim, String renameOpt, String quote) throws IOException{
		
		String id = delim + " " + renameOpt + " " + quote;
		String csvFile = TstConstants.TEMP_DIRECTORY + "zzz" +  ".csv";
		String[] args1= {
				"-I", Cbl2CsvCommonCode.DTAR020_FILE_NAME, 
				"-IFS", "Text",     /* File Structure  */
				"-O", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
				"-D", delim,
				"-Q", quote,                /* Quote           */
				"-Rename", renameOpt,
		}; /* Field Seperator will default to \t */
		ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(args1);
		

        ByteArrayOutputStream os = new ByteArrayOutputStream(Cbl2CsvCommonCode.LINES2_BYTES.length);
		
		Cobol2Csv.runCobol2Csv(csvArgs, iobCbl, 
				new ByteArrayInputStream(Cbl2CsvCommonCode.LINES2_BYTES), os);

		byte[] csvBytes = os.toByteArray();
		
		System.out.println();
		System.out.println();
		System.out.println(new String(csvBytes));
		System.out.println();

		chkFile(id, new InputStreamReader(new ByteArrayInputStream(csvBytes)), Cbl2CsvCommonCode.buildLines2(csvArgs));
		
		String binFile = csvFile + ".bin";
		String[] args2= {
				"-I", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
				"-D", delim,
				"-Q", quote,                /* Quote           */
				"-O", binFile, 
				"-OFS", "Text",     /* File Structure  */
				"-Rename", renameOpt,
		};
		
//		Csv2Cobol.main(args2);
		ByteArrayOutputStream osBin = new ByteArrayOutputStream(Cbl2CsvCommonCode.COBOL_LINES2.length * 2);
		Csv2Cobol.doCopy(new ParseArgsCobol2Csv(args2), iobCbl, new ByteArrayInputStream(csvBytes), osBin);
		
		String cobolData = new String(osBin.toByteArray());
		
		chkTrimFile(id, new StringReader(cobolData), Cbl2CsvCommonCode.COBOL_LINES2);
	}



	/**
	 * @param expected
	 * @param r
	 * @throws IOException
	 */
	public void chkFile(String id, Reader reader, String[] expected) throws IOException {
		String line;
		int i = 0;
		BufferedReader r = new BufferedReader(reader);
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(id, expected[i++], line);
		}	
		r.close();
	}

	/**
	 * @param expected
	 * @param r
	 * @throws IOException
	 */
	public void chkTrimFile(String id, Reader reader, String[] expected) throws IOException {
		String line;
		int i = 0;
		BufferedReader r = new BufferedReader(reader);
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(id, expected[i++], line.trim());
		}	
		r.close();
	}

}
