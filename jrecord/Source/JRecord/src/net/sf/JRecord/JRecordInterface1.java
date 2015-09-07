package net.sf.JRecord;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.IO.builders.CsvIOBuilder;
import net.sf.JRecord.IO.builders.FileSchemaBuilder;
import net.sf.JRecord.IO.builders.FixedWidthIOBuilder;
import net.sf.JRecord.IO.builders.SchemaIOBuilder;
import net.sf.JRecord.def.IO.builders.ICobolCopybookIOProvider;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOCopybookProvider;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;


/**
 * This class implements an easy to use interface to main JRecord functions. It will create <b>IOBuilders</b>
 * for a variety of schema-types, both those in files / streams
 * and those defined in Java Code. The variables available are<ul>
 * <li><b>COBOL</b> used to Create Cobol data file readers / writers
 * <li><b>CB2XML</b> used to create file reader / writers using a  Cb2xml Xml schema's
 * <li><b>SCHEMA_XML</b> used to create file reader / writers using a  RecordEditor-Xml schema's
 * <li><b>CSV</b> Create Csv readers / writers in java code.
 * <li><b>FIXED_WIDTH</b> Create Fixed-Width file readers / writers in java code (instead of from a Cobol-Copybook or Xml-Schema).
 * <li><b>SCHEMA</b> Create file readers / writers from an existing schema.
 * </ul>
 * 
 * <b>Example:</b><pre>{@code
 *      AbstractLineReader r = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 * }</pre> 
 * 
 * @author Bruce Martin
 *
 */
public class JRecordInterface1 {

	/**
	 * Create Reader's / Writers based on Cobol-Copybooks
	 * 
	 * <pre><b>Example:</b>
	 * {@code
	 *      AbstractLineReader r = JRecordInterface1.COBOL
	 *              .newIOBuilder("file-name")
	 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
	 *              .newReader("Data-Filename");
	 * }</pre> 
	 */
	public static final ICobolCopybookIOProvider  COBOL = new FileSchemaBuilder(CopybookLoaderFactory.COBOL_LOADER); 
	
	/**
	 * Create IO Builders for Cb2Xml-xml schemas (created from Cobol copybooks
	 * <br/><b><font color="blue">Note:</font> This option is new, it has undergone basic testing</b>
	 */
	public static final ICobolCopybookIOProvider CB2XML = new FileSchemaBuilder(CopybookLoaderFactory.CB2XML_LOADER); 

	/**
	 * Create IOBuilders for Record-Editor Xml descriptions
	 * <br/><b><font color="blue">Note:</font> This option is new, it has undergone basic testing</b>
	 */
	public static final IIOCopybookProvider  SCHEMA_XML = new FileSchemaBuilder(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER); 
	
	/** 
	 * Create a IOBuilder based on an existing Schema (LayoutDetail)
	 * <br/><b><font color="blue">Note:</font> This option is new, it has undergone basic testing</b>
	 */
	public static final SchemaIOBuilderProvider  SCHEMA = new SchemaIOBuilderProvider(); 

	/**
	 * Create Csv-IO-Builders
	 * <br/><b><font color="blue">Note:</font> This option is new, it has undergone basic testing</b>
	 */
	public static final CsvIOBuilderProvider        CSV = new CsvIOBuilderProvider();
	
	/**
	 * Create Fixed-Width-IO Builders
	 * <br/><b><font color="blue">Note:</font> This option is new, it has undergone basic testing</b>
	 */
	public static final FixedWidthIOBuilderProvider FIXED_WIDTH = new FixedWidthIOBuilderProvider();
	
	/**
	 * This class creates Csv-IO-Builders {@link CsvIOBuilder}
	 * @author Bruce Martin
	 *
	 */
	public static class CsvIOBuilderProvider {
		/**
		 * Create a Csv-IO-Builder {@link CsvIOBuilder} with Quote="\"" and field-separator=","
		 * @return Csv-IO-Builder {@link CsvIOBuilder}
		 */
		public ICsvIOBuilder newIOBuilder() {
			return CsvIOBuilder.newCsvIOBuilder();
		}
		
		/**
		 * Create a Csv-IO-Builder {@link CsvIOBuilder} with supplied Quote= and field-separator
		 * 
		 * @param delimiter field delimiter to use in the file
		 * @param quote Quote used to surround fields
		 * 
		 * @return Csv-IO-Builder {@link CsvIOBuilder}
		 */
		public ICsvIOBuilder newIOBuilder(String delimiter, String quote) {
			return CsvIOBuilder.newCsvIOBuilder(delimiter, quote);
		}
	}
	
	/**
	 * This class creates Fixed-Width IO Builders {@link IFixedWidthIOBuilder}
	 * @author Bruce Martin
	 *
	 */
	public static class FixedWidthIOBuilderProvider {
		public IFixedWidthIOBuilder newIOBuilder() {
			return FixedWidthIOBuilder.newFixedWidthIOBuilder();
		}
	}
	
	/**
	 * This class creates SchemaIOProviders {@link ISchemaIOBuilder}
	 * 
	 * @author Bruce Martin
	 *
	 */
	public static class SchemaIOBuilderProvider {
		public ISchemaIOBuilder newIOBuilder(LayoutDetail schema) {
			return SchemaIOBuilder.newSchemaIOBuilder(schema);
		}
	}
}
