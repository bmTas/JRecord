package net.sf.JRecord.zExamples.cobol.toCsv.sr;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.cbl2csv.Cobol2CsvInterface;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class TCobol2Csv2 {

	public static void main(String[] args) throws FileNotFoundException, IOException {

		ICobolIOBuilder iob = Cobol2CsvInterface.COBOL.newIOBuilder(Data.DTAR020_COPYBOOK_FILE_NAME)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
				.setFont("cp037");
		
		StringWriter sw = new StringWriter();
		try {
			Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(iob)
				.setInputFileName("/home/bruce/.RecordEditor/HSQLDB/SampleFiles/DTAR020_error.bin")
				.setFieldSeperator("!")
				.setGenerateHeader(true)
				.writeCsv(sw);
		} finally {
			System.out.println(sw);
		}

	}

}
