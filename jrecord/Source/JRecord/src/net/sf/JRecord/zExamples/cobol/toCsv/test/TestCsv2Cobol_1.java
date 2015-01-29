/**
 * 
 */
package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.zExamples.cobol.toCsv.Csv2Cobol;

/**
 * @author Bruce01
 *
 */
public class TestCsv2Cobol_1 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {

		String inputFileName1 = TestCsv2Cobol_1.class.getResource("DTAR020.csv").getFile();
		String[] args1 = {
				"-I", inputFileName1, 
				"-O", "G:\\Temp\\o_DTAR020.bin", 
				"-C", TestCsv2Cobol_1.class.getResource("DTAR020.cbl").getFile(), 
				"-Q", "\"",                /* Quote           */
				"-FS", "Fixed_Length",     /* File Structure  */
				"-OC", "CP273",            /* Input Character set   */
		}; /* Field Seperator will default to \t */
		
		Csv2Cobol.main(args1); 
		
		String inputFileName2 = TestCsv2Cobol_1.class.getResource("DTAR1000_Store_file_std_02.csv").getFile();
		String[] args2= {
				"-I", inputFileName2, 
				"-O", "G:\\Temp\\o_DTAR1000_Store_file_std_02.bin", 
				"-C", TestCobol2Csv02_2.class.getResource("DTAR1000.cbl").getFile(), 
				"-Q", "\"",               /* Quote           */
				"-FS", "Mainframe_VB",    /* File Structure  */
				"-OC", "CP273",           /* Character set   */
				"-D", ";"                 /* Field Seperator */ 
		}; /* Field Seperator will default to \t */
		
		Csv2Cobol.main(args2); 

	}

}
