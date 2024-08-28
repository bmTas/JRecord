package net.sf.JRecord.cbl2csv.create.builders;

import java.io.Reader;

import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr;
import net.sf.JRecord.cbl2csv.imp.mr.Cobol2CsvMrAdvancedBldr;

/**
 * Access Record Csv Builders.
 * 
 * @author Bruce Martin
 *
 */

public class MultiRecordFiles {
	/**
	 * Basic MultiRecord Csv Writer
	 * 
	 * @return Basic MultiRecord Csv Writer
	 */
	public ICobolToCsvBldr newMultiRecordCsvBuilder() {
		return new CobolToCsvBldr();
	}

	/**
	 * Convert a multi-record Cobol file into one or more Csv Files.
	 * 
	 * <pre>
	 * <b>Example:</b>
	 * 
	 * Cobol2CsvInterface.newAdvancedMultiRecordBuilder(poDownloadCobolFileName)
	 *         .setFont(Conversion.DEFAULT_ASCII_CHARSET)
	 *         .setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
	 *         .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
	 *         .setRecordDecider(
	 *                JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false) 
	 *                    .addRecord("H1", PO_HEADER_RECORD) .addRecord("D1", PRODUCT_RECORD)
	 *                    .addRecord("S1", LOCATION_RECORD) .setCaseSensitive(false) .build() )
	 * 
	 *          .newOutputDefinition(Data.AMS_PO_DATA)
	 *               .addRecordDefinition("/home/bruce/work/temp/PoHeader.csv", PO_HEADER_RECORD)
	 *                  .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *               .addRecordDefinition("/home/bruce/work/temp/PoProduct.csv", PRODUCT_RECORD)
	 *                  .addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO") .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *               .addRecordDefinition("/home/bruce/work/temp/PoLocation.csv", LOCATION_RECORD)
	 *                  .addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO")
	 *                  .addRecordFieldList(PRODUCT_RECORD, "APN", "Product") .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *          .writeCsvFiles()
	 * 
	 * </pre
	 * 
	 * @param copybookName
	 * @return new Builder
	 */
	public Cobol2CsvMrAdvancedBldr newAdvancedMultiRecordBuilder(String copybookName) {
		return Cobol2CsvMrAdvancedBldr.newCsvWriter(copybookName);
	}

	/**
	 * Convert a multi-record Cobol file into one or more Csv Files.
	 * 
	 * <pre>
	 * <b>Example:</b>
	 * 
	 * Cobol2CsvInterface.MULTI_RECORD_FILES.newAdvancedMultiRecordBuilder(
	 *                                    new FileReader(poDownloadCobolFileName), "copbook-name")
	 *         .setFont(Conversion.DEFAULT_ASCII_CHARSET)
	 *         .setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
	 *         .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
	 *         .setRecordDecider(
	 *                JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false) 
	 *                    .addRecord("H1", PO_HEADER_RECORD) .addRecord("D1", PRODUCT_RECORD)
	 *                    .addRecord("S1", LOCATION_RECORD) .setCaseSensitive(false) .build() )
	 * 
	 *          .newOutputDefinition(Data.AMS_PO_DATA)
	 *               .addRecordDefinition("/home/bruce/work/temp/PoHeader.csv", PO_HEADER_RECORD)
	 *                  .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *               .addRecordDefinition("/home/bruce/work/temp/PoProduct.csv", PRODUCT_RECORD)
	 *                  .addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO") .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *               .addRecordDefinition("/home/bruce/work/temp/PoLocation.csv", LOCATION_RECORD)
	 *                  .addRecordFieldList(PO_HEADER_RECORD, "Vendor", "PO")
	 *                  .addRecordFieldList(PRODUCT_RECORD, "APN", "Product") .addAllFields()
	 *               .endRecordDefinition()
	 * 
	 *          .writeCsvFiles()
	 * 
	 * </pre
	 * 
	 * @param copybookReader Reader for a Cobol Copybook
	 * @param copybookName   Name of the copybook
	 * @return new Builder
	 */
	public Cobol2CsvMrAdvancedBldr newAdvancedMultiRecordBuilder(Reader copybookReader, String copybookName) {
		return Cobol2CsvMrAdvancedBldr.newCsvWriter(copybookReader, copybookName);
	}

}
