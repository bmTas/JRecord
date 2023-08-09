package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class MulRecCobol2Csv02 {

	public static void main(String[] args) throws IOException {
		String poDownloadCobolFileName = Data.AMS_PO_COBOL_COPYBOOK_FILE_NAME;
		URL resource = Data.AMS_PO_DATA_RESOUCE;
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(poDownloadCobolFileName)
					.setFont(Conversion.DEFAULT_ASCII_CHARSET)
					.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
					.setRecordDecider(JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false)
									.addRecord("H1", "PO-Record")
									.addRecord("D1", "Product-Record")
									.addRecord("S1", "Location-Record")
									.setCaseSensitive(false)
								.build()
					);
				
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator(";")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setOutputFile("/home/bruce/work/temp/semiAmsPo_1.csv")
				.run();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setWriteRecordName(true)
					.setSeparator(";")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setOutputFile("/home/bruce/work/temp/semiAmsPo_2.csv")
				.run();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(false)
					.setSeparator(";")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setOutputFile("/home/bruce/work/temp/semiAmsPo_3.csv")
				.run();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator(":")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setOutputFile("/home/bruce/work/temp/colonAmsPo_{record}.csv", "{record}")
				.run();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setOutputCharacterSet("utf16")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setOutputFile("/home/bruce/work/temp/tabAmsPo_{record}.csv", "{record}")
				.run();
		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator(",")
					.setLineReader(ioBldr.newReader(resource.openStream()))
					.setCsvWriter(new BufferedWriter(new FileWriter("/home/bruce/work/temp/commaAmsPo_Default.csv")))
					.addRecordDetails("Location-Record", new BufferedWriter(new FileWriter("/home/bruce/work/temp/commaAmsPo_Location.csv")))
				.run();
	}

}
