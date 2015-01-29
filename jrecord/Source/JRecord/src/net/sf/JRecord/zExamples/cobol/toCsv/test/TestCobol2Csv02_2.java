/**
 * 
 */
package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.zExamples.cobol.toCsv.Cobol2Csv;

/**
 * @author Bruce01
 *
 */
public class TestCobol2Csv02_2 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {
		String inputFileName = TestCobol2Csv02_2.class.getResource("DTAR1000_Store_file_std.bin").getFile();
		String[] args= {
				"-I", inputFileName, 
				"-O", "G:\\Temp\\DTAR1000_Store_file_std_02.csv", 
				"-C", TestCobol2Csv02_2.class.getResource("DTAR1000.cbl").getFile(), 
				"-Q", "\"",               /* Quote           */
				"-FS", "Mainframe_VB",    /* File Structure  */
				"-IC", "CP273",           /* Character set   */
				"-D", ";"                 /* Field Seperator */ 
		}; /* Field Seperator will default to \t */
		
		Cobol2Csv.main(args); 	}

}
