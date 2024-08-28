package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class MulRecCobol2Csv11 {

	public static void main(String[] args) throws IOException {
		StringWriter sw = new StringWriter();
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(Data.DTAR020_COPYBOOK_FILE_NAME)
					.setFont("cp037")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH);
				
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator("\t")
					.setLineReader(ioBldr.newReader(Data.DTAR020_CSVTST_RESOURCE.openStream()))
					.setCsvWriter(new BufferedWriter(sw))
					.setReportInvalidFields(true)
				.run();
		
		System.out.println(sw.toString());
		System.out.println();
		System.out.println();
		System.out.println();
		System.out.println();
		
		sw = new StringWriter();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
				.setCsvHeader(true)
				.setSeparator("|")
				.setLineReader(ioBldr.newReader(Data.DTAR020_CSVTST_RESOURCE.openStream()))
				.setCsvWriter(new BufferedWriter(sw))
				.setLowValueTxt("Null")
				.setNumericSpacesTxt(" ")
			.run();
		System.out.println(sw.toString());

	}

}
