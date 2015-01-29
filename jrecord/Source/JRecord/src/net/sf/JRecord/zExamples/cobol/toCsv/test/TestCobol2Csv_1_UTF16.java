/**
 * 
 */
package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2CsvAlternative;

/**
 * @author Bruce01
 *
 */
public class TestCobol2Csv_1_UTF16 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {

		String inputFileName = TestCobol2Csv_1_UTF16.class.getResource("DTAR020.bin").getFile();
		String[] args= {
				"-I", inputFileName, 
				"-O", "G:\\Temp\\DTAR020_UTF16.csv", 
				"-C", TestCobol2Csv_1_UTF16.class.getResource("DTAR020.cbl").getFile(), 
				"-Q", "\"",                /* Quote           */
				"-FS", "Fixed_Length",     /* File Structure  */
				"-IC", "CP273",            /* Input Character set  */
				"-OC", "UTF-16",           /* Output Character set  */
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvAlternative.main(args); 
	}

}
