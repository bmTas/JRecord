package net.sf.JRecord.cbl2csv.create.builders;

import java.io.IOException;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.cbl2csv.imp.sr.Cobol2CsvSrBuilder;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

/**
 * Access Single Record Csv Builders.
 * 
 * @author Bruce Martin
 *
 */

public class SingleRecordFiles {
	/**
	 * Create a Csv-Writer from a JRecord-Layout (file Schema)
	 * 
	 * <pre>
	 *<b>Example:</b>
	 * 
	 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface
	 *              .<b>newIOBuilder(new FileReader"file-name"), "Copybook-Name")</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_MAINFRAME));
	 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder.getLayout())
	 * </pre>
	 * 
	 * @param schema JRecord-File-Schema
	 * @return Single-Record-Type Csv-Writer
	 */
	public Cobol2CsvSrBuilder newCobol2CsvSingleRecordBuilder(LayoutDetail schema) {
		return new Cobol2CsvSrBuilder(schema);
	}

	/**
	 * Create a Csv-Writer from a JRecord-IoBuilder
	 * 
	 * <pre>
	 *<b>Example:</b>
	 * 
	 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface
	 *              .<b>newIOBuilder(new FileReader"file-name"), "Copybook-Name")</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_MAINFRAME));
	 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder)
	 * </pre>
	 * 
	 * @param ioBuilder JRecord IoBuilder
	 * @return Single-Record-Type Csv-Writer
	 * @throws IOException
	 */
	public Cobol2CsvSrBuilder newCobol2CsvSingleRecordBuilder(ISchemaIOBuilder ioBuilder) throws IOException {
		return new Cobol2CsvSrBuilder(ioBuilder);
	}

}
