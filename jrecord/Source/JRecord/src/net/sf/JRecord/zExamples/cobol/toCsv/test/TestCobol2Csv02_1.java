/**
 * 
 */
package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2Csv;

/**
 * @author Bruce01
 *
 */
public class TestCobol2Csv02_1 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {

		String inputFileName = TestCobol2Csv02_1.class.getResource("DTAR020.bin").getFile();
		String[] args= {
				"-I", inputFileName, 
				"-O", "G:\\Temp\\DTAR020_02.csv", 
				"-C", TestCobol2Csv02_1.class.getResource("DTAR020.cbl").getFile(), 
				"-Q", "\"",                /* Quote           */
				"-FS", "Fixed_Length",     /* File Structure  */
				"-IC", "CP273",            /* Character set   */
		}; /* Field Seperator will default to \t */
		
		Cobol2Csv.main(args); 
	}

}
