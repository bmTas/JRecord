package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.List;

import com.fasterxml.jackson.core.JsonParseException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.IArrayItemCheck;


/**
 * Class To convert <i>Cobol Data Files</i> to/from <i>Json Data files</i> using a Cobol Copybook,
 * This class defines a "Builder" interface for loading the Cobol Copybook
 *  
 * @author Bruce Martin
 *
 */
public interface ICobol2Json  extends  Icb2xml2Json  {
//	public static final String MAIN_XML_TAG = "CobolData";

	@Override public abstract ICobol2Json setFileOrganization(int fileOrganization);

	@Override public abstract ICobol2Json setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */

	public abstract ICobol2Json setDialect(int dialect);

	@Override public abstract ICobol2Json setFont(String font);

	@Override public abstract ICobol2Json setInitToSpaces(boolean initToSpaces);

	@Override public abstract ICobol2Json setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	@Override public abstract ICobol2Json setRecordDecider(RecordDecider recordDecider);

	/**
	 * {@inheritDoc}
	 */
	@Override public abstract ICobol2Json setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

	@Override public abstract ICobol2Json setCopybookFileFormat(int copybookFileFormat);

	@Override public abstract ICobol2Json setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);
	
	@Override public ICobol2Json setPrettyPrint(boolean prettyPrint);


	
	/**
	 * {@inheritDoc}
	 */
	@Override 
	public abstract ICobol2Json setArrayCheck(String arrayName, IArrayItemCheck check);

	@Override
	public abstract ICobol2Json setTagFormat(int tagFormat);

//	/**
//	 * Set the main <i>element</i> name in the generated Json. By default this is "CobolData"
//	 * 
//	 * @param xmlMainElement name of the main element
//	 */
//	public abstract ICobol2Json setXmlMainElement(String xmlMainElement);
//	public ICobol2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);
	
//	/**
//	 * Define the parent record.
//	 * 
//	 * @param recordName record name
//	 * @param parentName name of the parent record.
//	 * 
//	 * @return this
//	 */
	@Override
	public abstract ICobol2Json setRecordParent(String recordName, String parentName);
	
	@Override
	public abstract ICobol2Json setRootRecord(String recordName);
	
	/**
	 * 
	 * @return an IOBuilder related to the Cobol2Xml class
	 */
	public ISchemaIOBuilder asIOBuilder();

	/**
	 * Write a Coboi Stream data stream to a writer 
	 * @param cobolStream Cobol Data Stream
	 * @param writer where json is written to
	 * @throws IOException
	 */
	void cobol2json(InputStream cobolStream, Writer writer) throws IOException;

	/**
	 * Convert a Single Cobol Record to the equivalent JSon
	 * @param cobolData cobol data (byte array)
	 * @param writer where the JSon is to be written
	 * @throws IOException any IOException
	 */
	void singleCobolRecord2json(byte[] cobolData, Writer jsonWriter) throws IOException;

	/**
	 * Convert a Single Cobol Record to the equivalent Json String
	 * @param cobolData Cobol data (array of bytes)
	 * @return json string generated from the Cobol data 
	 * @throws IOException
	 */
	String singleCobolRecord2jsonString(byte[] cobolData) throws IOException;

	/**
	 * Convert a json String to a Cobol record (as a byte array)
	 * @param json json String
	 * @return equivalent Cobol record (as a byte array)
	 * 
	 * @throws JsonParseException
	 * @throws IOException
	 */
	byte[] jsonStringToSingleCobolRecord(String json) throws JsonParseException, IOException;

	/**
	 * Convert json to a Single Cobol record (as a stream of bytes) 
	 * @param jsonReader a reader for the json String
	 * @param outStream stream to write the cobol data
	 * @return The Cobol2Json object for more updates
	 * @throws JsonParseException
	 * @throws IOException
	 */
	ICobol2Json jsonObjectToCobolFile(Reader jsonReader, OutputStream outStream) throws JsonParseException, IOException;

	/**
	 * Convert a json object to a single Cobol Record
	 * @param jsonFileName name of the <i>json file</i> to be read
	 * @param outFileName name of the <i>Cobol data file</i> where the Cobol data is to be written
	 * @return Cobol2json object so more conversions can be run
	 * 
	 * @throws JsonParseException
	 * @throws IOException
	 */
	ICobol2Json jsonObjectToCobolFile(String jsonFileName, String outFileName) throws JsonParseException, IOException;

	/**
	 * Convert a json array to a multiple Cobol Records
	 * 
	 * @param jsonReader reader to read the json from
	 * @param outStream Stream to write the Cobol Data to
	 * @return  Cobol2json object so more conversions can be run
	 * 
	 * @throws JsonParseException
	 * @throws IOException
	 */
	ICobol2Json jsonArrayToCobolFile(Reader jsonReader, OutputStream outStream) throws JsonParseException, IOException;

	/**
	 * Convert a json array to a multiple Cobol Records
	 * 
	 * @param jsonFileName Name of File to read the Cobol data from
	 * @param outFileName Name of File to write Cobol data to
	 * @returnCobol2json object so more conversions can be run
	 * 
	 * @throws JsonParseException
	 * @throws IOException
	 */
	ICobol2Json jsonArrayToCobolFile(String jsonFileName, String outFileName) throws JsonParseException, IOException;

	/**
	 * Convert JSon to a list of lines
	 * @param jsonReader wherethe json is to be read from
	 * @return List of Lines
	 * @throws JsonParseException
	 * @throws IOException
	 */
	List<AbstractLine> jsonArrayToCobolLines(Reader jsonReader)
			throws JsonParseException, IOException;

	
//	/**
//	 * Convert Cobol Data File to Json file
//	 * 
//	 * @param cobolFileName input Cobol-Data file name
//	 * @param xmlFileName output Json-Data file name
//	 * @throws FileNotFoundException 
//	 * 
//	 * @throws IOException 
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void cobol2json(String cobolFileName, String xmlFileName) throws IOException, JAXBException;
//	
//	/**
//	 * Convert Cobol Data File to Json file
//	 * 
//	 * @param cobolStream
//	 * @param xmlStream
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void cobol2json(InputStream cobolStream, OutputStream xmlStream) throws IOException, JAXBException;
//
//	/**
//	 * Convert Input Json-Data to Cobol Data-File
//	 * 
//	 * @param xmlFileName Input Json-File name 
//	 * @param cobolFileName Ouput Cobol-Data File name
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void json2Cobol(String xmlFileName, String cobolFileName)
//			throws IOException,  XMLStreamException;
//
//	/**
//	 * Convert a Json-Data in to a Cobol Data 
//	 * @param xmlStream Input Json Data
//	 * @param cobolStream Output Cobol data
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void json2Cobol(InputStream xmlStream, OutputStream cobolStream)
//			throws IOException, 
//			XMLStreamException;
//
}
