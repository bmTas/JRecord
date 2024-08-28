package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cbl2csv.imp.mr.Cobol2CsvMrAdvancedBldr;
import net.sf.JRecord.zData.Data;

public class AdvancedMultiRecordExport {
	private static final String LOCATION_RECORD = "Location-Record";
	private static final String PRODUCT_RECORD = "Product-Record";
	private static final String PO_HEADER_RECORD = "PO-Record";

	public static void main(String[] args) throws IOException {
		String poDownloadCobolFileName = Data.AMS_PO_COBOL_COPYBOOK_FILE_NAME;
		
		Cobol2CsvMrAdvancedBldr.newCsvWriter(poDownloadCobolFileName)
				.setFont(Conversion.DEFAULT_ASCII_CHARSET)
				.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
				.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
				.setRecordDecider(
						JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false)
								.addRecord("H1", PO_HEADER_RECORD)
								.addRecord("D1", PRODUCT_RECORD)
								.addRecord("S1", LOCATION_RECORD)
								.setCaseSensitive(false)
								.build()
					)
				
				.newOutputDefinition(Data.AMS_PO_DATA)
					.addRecordDefinition("/home/bruce/work/temp/PoHeader.csv", PO_HEADER_RECORD)
						.addAllFields()
						.endRecordDefinition()
						
					.addRecordDefinition("/home/bruce/work/temp/PoProduct.csv", PRODUCT_RECORD)
						.addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO")
						.addAllFields()
						.endRecordDefinition()
						
					.addRecordDefinition("/home/bruce/work/temp/PoLocation.csv", LOCATION_RECORD)
						.addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO")
						.addRecordFieldList(PRODUCT_RECORD, "APN", "Product")
						.addAllFields()
						.endRecordDefinition()
						
					.writeCsvFiles()	
					
				;
	}
}
