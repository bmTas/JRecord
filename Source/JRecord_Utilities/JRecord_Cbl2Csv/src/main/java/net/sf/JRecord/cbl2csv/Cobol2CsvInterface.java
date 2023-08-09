package net.sf.JRecord.cbl2csv;

import net.sf.JRecord.cbl2csv.create.builders.CobolInterface;
import net.sf.JRecord.cbl2csv.create.builders.MultiRecordFiles;
import net.sf.JRecord.cbl2csv.create.builders.SingleRecordFiles;

/**
 * This class provides an interface to the various Builders available for
 * converting Cobol Data Files to/from Csv files
 * 
 * <b>Example:</b>
 * 
 * <pre>
 * 
 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface.COBOL
 *              .<b>newIOBuilder("file-name")</b>
 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU));
 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder.getLayout)
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public class Cobol2CsvInterface {
	
	public static final CobolInterface COBOL = new CobolInterface();

	public static final SingleRecordFiles SINGLE_RECORD_FILES = new SingleRecordFiles();

	public static final MultiRecordFiles MULTI_RECORD_FILES = new MultiRecordFiles();

	
}
