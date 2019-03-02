package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.cbl2csv.Cobol2CsvMultiRecord;
import net.sf.JRecord.zData.Data;

public class MulRecCobol2Csv04 {

	public static void main(String[] args) {
//		String cobolCopybook = MulRecCobol2Csv04.class.getResource("amsPoDownload.cbl").getFile();
//		String inputFileName = MulRecCobol2Csv04.class.getResource("Ams_PODownload_20041231.txt").getFile();
		String cobolCopybook = Data.AMS_PO_COBOL_COPYBOOK_FILE_NAME;
		String inputFileName = Data.AMS_PO_DATA;
		String[] args1= {
				"-I", inputFileName, 
				"-O", "/home/bruce/work/temp/batchSemmiAmsPo_1.csv", 
				"-C", cobolCopybook, 
				"-Q", "'",                 /* Quote           */
				"-FS", "Byte_Text",        /* File Structure  */
				"-IC", "",                 /* Character set   */
				"-D", ";",                 /* Field Separator */
				"-addRecordName", "no",
				"-split", "01",
				"-recordSelection", "PO-Record", "Record-Type=H1",
				"-recordSelection", "Product-Record", "Record-Type=D1",
				"-recordSelection", "Location-Record", "Record-Type=S1",
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvMultiRecord.main(args1); 
		
		String[] args2= {
				"-I", inputFileName, 
				"-O", "/home/bruce/work/temp/batchCommaAmsPo_2.csv", 
				"-C", cobolCopybook, 
				"-Q", "'",                 /* Quote           */
				"-FS", "Byte_Text",        /* File Structure  */
				"-IC", "",                 /* Character set   */
				"-D", ",",                 /* Field Separator */
				"-addRecordName", "yes",
				"-split", "01",
				"-recordSelection", "PO-Record", "Record-Type=H1",
				"-recordSelection", "Product-Record", "Record-Type=D1",
				"-recordSelection", "Location-Record", "Record-Type=S1",
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvMultiRecord.main(args2); 
		
		String[] args3= {
				"-I", inputFileName, 
				"-O", "/home/bruce/work/temp/batchTabAmsPo_3_{record}.csv", 
				"-C", cobolCopybook, 
				"-Q", "'",                 /* Quote           */
				"-FS", "Byte_Text",        /* File Structure  */
				"-IC", "",                 /* Character set   */
				//"-D", ";",                 /* Field Separator */
				"-addRecordName", "no",
				"-split", "01",
				"-recordSelection", "PO-Record", "Record-Type=H1",
				"-recordSelection", "Product-Record",  "Record-Type=D1",
				"-recordSelection", "Location-Record", "Record-Type=S1",
		}; /* Field Seperator will default to \t */
		
		Cobol2CsvMultiRecord.main(args3); 


	}

}
