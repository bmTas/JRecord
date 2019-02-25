package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.cbl2csv.Cobol2CsvMultiRecord;
import net.sf.JRecord.zData.Data;

public class MulRecCobol2Csv03 {

	public static void main(String[] args) {
		String inputFileName = Data.DTAR020_BIN_RESOURCE.getFile();
		String[] args1= {
				"-I", inputFileName, 
				"-O", "/home/bruce/work/temp/DTAR020_03.csv", 
				"-C", Data.DTAR020_COPYBOOK_FILE_NAME, 
				"-Q", "\"",                /* Quote           */
				"-FS", "Fixed_Length",     /* File Structure  */
				"-IC", "CP273",            /* Character set   */
				"-D", "|"                  /* Field Separator */ 
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvMultiRecord.main(args1); 
		System.out.println(ExampleConstants.TEMP_DIR );
	}

}
