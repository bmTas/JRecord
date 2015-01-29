package net.sf.JRecord.zExamples.cobol.specialCases;

import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TestCommonCode;

/**
 * This example tests seting a field to hex 
 * It was created to illistrate the process for a user of JRecord.
 *
 * @author Bruce Martin
 *
 */
public class Xmpl_Set {

	private static String cobolCopybook
			= "      01 COMPANY-RECORD.\n"
			+ "         05 COMPANY-NAME     PIC X(30).\n"
			+ "         05 EMPLOYEE-LIST.\n"
			+ "            10 PRESIDENT.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n";


	public static void main(String[] args) throws RecordException, IOException {
			// Create an Internal JRecord schema (or layout) from the cobol copybook
		LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
				cobolCopybook, "COMPANY-RECORD",
				CopybookLoader.SPLIT_NONE, "", Convert.FMT_INTEL);
		int recordIdx = 0;  // since there is only one record type, the record index must be zero
		                    // If there where more than one record, you could use:
		                    //    schema.getRecordIndex("COMPANY-RECORD");

			// Retrieve the Record-Definition
		RecordDetail record = schema.getRecord(recordIdx);

		Line line = new Line(schema);
		int stateFieldIndex  = record.getFieldIndex("STATE");
		
		char[] c = new char[record.getField(stateFieldIndex).getLen()];
		Arrays.fill(c, 'F');
		
		line.setFieldHex(0, stateFieldIndex, new String(c));
	}
}
