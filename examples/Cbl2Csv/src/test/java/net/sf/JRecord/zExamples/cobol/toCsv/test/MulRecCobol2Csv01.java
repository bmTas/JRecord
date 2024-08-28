package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class MulRecCobol2Csv01 {

	public static void main(String[] args) throws IOException {
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(Data.DTAR020_COPYBOOK_FILE_NAME)
					.setFont("cp037")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH);
				
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator("|")
					.setLineReader(ioBldr.newReader(Data.DTAR020_BIN_RESOURCE.openStream()))
					.setOutputFile("/home/bruce/work/temp/barDTAR020.csv")
				.run();
	}

}
