package net.sf.JRecord.def.IO.builders;


import java.io.InputStream;
import java.io.OutputStream;

import net.sf.JRecord.External.ExternalRecord;

/**
 * Interface to create IOBuilders
 * <pre>
 *     IIOBuilder ioBldr = JRecordInterface1.SCHEMA_XML
 *                                          .newIOBuilder(xmlSchemaName);
 * </pre>                                          
 * @author Bruce Martin
 *
 */
public interface IIOCopybookProvider {

	/**
	 * Create a new RecordEditor-Xml IOBuilder for a file (Default is Mainframe Cobol.
	 * 
	 * @param schemaFileName name of the Copybook (or schema file).
	 * 
	 * These are the default values (which can be overriden with the appropriate set* method
	 * @return requested IOBuilder
	 */
	public abstract IIOBuilder newIOBuilder(String schemaFileName);

	/**
	 * Create a new Cobol IOBulder for a file.
	 * 
	 * @param schemaStream stream to read the file-Schema from.
	 * @param schemaName name of the File-Schema.
	 * 
	 * These are the default values (which can be overriden with the appropriate set* method
	 * @return requested IOBuilder
	 */
	public abstract IIOBuilder newIOBuilder(
			InputStream schemaStream, String schemaName);
	
	/**
	 * Exporting a {@link ExternalRecord} to an Xml-File
	 * 
	 * @param fileName Output file name
	 * @param schema schema to export
	 * 
	 * @throws Exception any error that occurs
	 */
	public abstract void export(String fileName, ExternalRecord schema) throws Exception;

	/**
	 * Exporting a {@link ExternalRecord} to a Stream
	 * 
	 * @param outStream output stream where the schema is to be written
	 * @param schema
	 * 
	 * @throws Exception Any Error that occurs
	 */
	public abstract void export(OutputStream outStream, ExternalRecord schema) throws Exception;

}