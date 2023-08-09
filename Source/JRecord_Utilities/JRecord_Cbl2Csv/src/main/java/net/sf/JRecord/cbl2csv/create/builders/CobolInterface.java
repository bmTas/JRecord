package net.sf.JRecord.cbl2csv.create.builders;

import java.io.Reader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.copybookReader.ICobolCopybookTextSource;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

public class CobolInterface {
	/**
	 * Create a new Cobol IOBulder from a COBOL-Copybook file
	 * 
	 * <pre>
	 *<b>Example:</b>
	 * 
	 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface.COBOL
	 *              .<b>newIOBuilder("file-name")</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_MAINFRAME));
	 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder.getLayout())
	 * </pre>
	 * 
	 * @param copybookFileame name of the COBOL-Copybook stream.
	 * @return
	 */
	public ICobolIOBuilder newIOBuilder(String copybookFileame) {
		return JRecordInterface1.COBOL.newIOBuilder(copybookFileame);
	}

	/**
	 * 
	 * <pre>
	 *<b>Example:</b>
	 * 
	 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface.COBOL
	 *              .<b>newIOBuilder(new FileReader("file-name"), "Copybook-Name")</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_MAINFRAME));
	 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder.getLayout)
	 * </pre>
	 * 
	 * @param copybookReader Reader for the Cobol Copybook
	 * @param copybookName   copybook name
	 * 
	 * @return IoBuilder
	 */
	public ICobolIOBuilder newIOBuilder(Reader copybookReader, String copybookName) {
		return JRecordInterface1.COBOL.newIOBuilder(copybookReader, copybookName);
	}

	/**
	 * Create a IoBuilder from cb2xml Copybook reader
	 * 
	 * @param copyybookReader cb2xml CopyBookReader
	 * @return
	 */
	public ICobolIOBuilder newIOBuilder(ICobolCopybookTextSource copyybookReader) {
		return JRecordInterface1.COBOL.newIOBuilder(copyybookReader);
	}

	/**
	 * You use this class when<ul>
	 * <li>You want to combine multiple copyooks
	 * <li>You need to support the Cobol copy Statement (replacing option is not supported)
	 * </ul>
	 * 
	 * Usage:
	 * <pre>
	 *		ReadCobolCopybook copybook = Cobol2CsvInterface.COBOL.newCobolCopybookReader()
	 *				.setDirectoriesToSearch("/home/bruce/work/Cobol/CopyBooks")
	 *				.setColumns(CopybookColumns.STANDARD_COLUMNS)
	 *				.addCobolCopybook("/home/bruce/work/Cobol/Main01.cbl")
	 *				.setColumns(CopybookColumns.FREE_FORMAT)	// load following copybooks as freeformat	
	 *				.addCobolCopybook("/home/bruce/work/Cobol/Main02.cbl")
	 *				.addCobolCopybook(new StringReader("\n"
	 *						+ "   03 Extra-Field-01   pic x(10)."
	 *						+ "   03 Extra-Field-02   pic s9(7)v99 comp-3."))
	 *				;
	 *
	 *      ICobolIOBuilder ioBuilder = Cobol2CsvInterface.COBOL
	 *              .<b>newIOBuilder(copybook)</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_MAINFRAME));
	 *      Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(ioBuilder.getLayout())
	 * </pre> 
	 * @return Class to Read one or more <i>Cobol Copybooks</i>.
	 */
	public ReadCobolCopybook newCobolCopybookReader() {
		return new ReadCobolCopybook();
	}

}
