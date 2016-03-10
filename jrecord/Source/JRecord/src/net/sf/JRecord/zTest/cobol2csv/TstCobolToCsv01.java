package net.sf.JRecord.zTest.cobol2csv;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2Csv;
import net.sf.JRecord.zExamples.cobol.toCsv.Csv2Cobol;
import net.sf.JRecord.zExamples.cobol.toCsv.ParseArgsCobol2Csv;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;


/**
 * Basic Tests using the "Batch" interface
 * Other programs will large volume tests without accessing the Disks
 * @author Bruce Martin
 *
 */
public class TstCobolToCsv01 extends TestCase {

	
	public void testCsv01() throws IOException {
		tstCsv("1", "!", "Asis");
	};
	
	
	public void testCsv02() throws IOException {
		tstCsv("2", ";", "_");
	};
	
	
	public void testCsv03() throws IOException {
		tstCsv("3", ",", "no-");
	};
	public void tstCsv(String id, String delim, String renameOpt) throws IOException{
		String csvFile = TstConstants.TEMP_DIRECTORY + "DTAR020_tst1" + id + ".csv";
		String[] args1= {
				"-I", Cbl2CsvCommonCode.DTAR020_FILE_NAME, 
				"-IFS", "Fixed_Length",     /* File Structure  */
				"-IC", "CP037",            /* Character set   */
				"-O", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "DTAR020.cbl", 
				"-D", delim,
				"-Q", "\"",                /* Quote           */
				"-Rename", renameOpt,
		}; /* Field Seperator will default to \t */
		
		Cobol2Csv.main(args1); 

		ParseArgsCobol2Csv argObj = new ParseArgsCobol2Csv(args1);
		String[] colNames = Cbl2CsvCommonCode.getColNames(argObj);
		

		chkFile(csvFile, Cbl2CsvCommonCode.buildLines(delim, colNames));
		
		String binFile = csvFile + ".bin";
		String[] args2= {
				"-I", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "DTAR020.cbl", 
				"-D", delim,
				"-Q", "\"",                /* Quote           */
				"-O", binFile, 
				"-OFS", "Fixed_Length",     /* File Structure  */
				"-OC", "CP037",            /* Character set   */
				"-Rename", renameOpt,
		};
		
		Csv2Cobol.main(args2);
		
		byte[] fileContents = TestCommonCode.loadFile(binFile);
		
		boolean equals = Arrays.equals(Cbl2CsvCommonCode.DTAR020_BYTES, fileContents);
		if (! equals) {
			System.out.println("Lengths: " + Cbl2CsvCommonCode.DTAR020_BYTES.length + ", " + fileContents.length);
			assertTrue(equals);
		}
	}
	

	public void chkFile(String csvFile, String[] expected) throws IOException {
		BufferedReader r = new BufferedReader(new FileReader(csvFile));
		String line;
		int i = 0;
		
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(expected[i++], line);
		}	
		r.close();
	}

}
