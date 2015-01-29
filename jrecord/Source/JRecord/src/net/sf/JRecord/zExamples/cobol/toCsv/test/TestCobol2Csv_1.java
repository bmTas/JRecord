/**
 * 
 */
package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2CsvAlternative;

/**
 * @author Bruce01
 *
 */
public class TestCobol2Csv_1 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {

		String inputFileName = TestCobol2Csv_1.class.getResource("DTAR020.bin").getFile();
		String[] args= {
				"-I", inputFileName, 
				"-O", "G:\\Temp\\DTAR020.csv", 
				"-C", TestCobol2Csv_1.class.getResource("DTAR020.cbl").getFile(), 
				"-Q", "\"",                /* Quote           */
				"-FC", "Fixed_Length",     /* File Structure  */
				"-IC", "CP037",            /* Input Character set   */
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvAlternative.main(args); 
	}

}
