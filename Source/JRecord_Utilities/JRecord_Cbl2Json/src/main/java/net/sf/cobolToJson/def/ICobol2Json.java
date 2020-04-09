package net.sf.cobolToJson.def;

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
