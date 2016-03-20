package net.sf.JRecord.cbl2xml.def;

import java.io.IOException;
import java.io.InputStream; 
import java.io.OutputStream;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.IArrayItemCheck;


/**
 * Class To convert <i>Cobol Data Files</i> to/from <i>Xml Data files</i> using a Cobol Copybook,
 * This class defines a "Builder" interface for loading the Cobol Copybook
 *  
 * @author Bruce Martin
 *
 */
public interface ICobol2Xml  extends  Icb2xml2Xml  {
	public static final String MAIN_XML_TAG = "CobolData";

	@Override public abstract ICobol2Xml setFileOrganization(int fileOrganization);

	@Override public abstract ICobol2Xml setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */

	public abstract ICobol2Xml setDialect(int dialect);

	@Override public abstract ICobol2Xml setFont(String font);

	@Override public abstract ICobol2Xml setInitToSpaces(boolean initToSpaces);

	@Override public abstract ICobol2Xml setRecordSelection(String recordName, ExternalSelection selectionCriteria);
	
	/**
	 * {@inheritDoc}
	 */
	@Override public abstract ICobol2Xml setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

	@Override public abstract ICobol2Xml setCopybookFileFormat(int copybookFileFormat);

	@Override public abstract ICobol2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	@Override public abstract ICobol2Xml setArrayCheck(String arrayName, IArrayItemCheck check);

	@Override public abstract ICobol2Xml setXmlInputFactory(XMLInputFactory xmlInputFactory);
	
	@Override public abstract ICobol2Xml setXmlOutputFactory(XMLOutputFactory xmlOutputFactory);

	/**
	 * Set The Format  of Xml
	 * @param tagFormat How to format Cobol-names as Xml-Tags, valuies<ul>
	 * <li><b>Cbl2XmlValues.RO_LEAVE_ASIS</b> (Default) Keep the Cobol variable name
	 * <li><b>Cbl2XmlValues.RO_MINUS_TO_UNDERSCORE</b> Convert Minus (-) to underscore (_) in Cobol name.
	 * Cobol-Var-Name ==&gt; Cobol_Var_Name
	 * <li><b>Cbl2XmlValues.RO_CAMEL_CASE</b> Camel case conversion Cobol-Var-Name ==&gt; cobolVarName
	 * </ul>
	 * @return this 
	 */
	public abstract ICobol2Xml setTagFormat(int tagFormat);

	/**
	 * Set the main <i>element</i> name in the generated Xml. By default this is "CobolData"
	 * 
	 * @param xmlMainElement name of the main element
	 */
	public abstract ICobol2Xml setXmlMainElement(String xmlMainElement);
//	public ICobol2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);
	
	/**
	 * Define the parent record.
	 * 
	 * @param recordName record name
	 * @param parentName name of the parent record.
	 * 
	 * @return this
	 */
	public abstract ICobol2Xml setRecordParent(String recordName, String parentName);
	
	/**
	 * Convert Cobol Data File to Xml file
	 * 
	 * @param cobolFileName input Cobol-Data file name
	 * @param xmlFileName output Xml-Data file name
	 * 
	 * @throws IOException 
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void cobol2xml(String cobolFileName, String xmlFileName)  
			throws  IOException, JAXBException, XMLStreamException;
	
	/**
	 * Convert Cobol Data File to Xml file
	 * 
	 * @param cobolStream
	 * @param xmlStream
	 * 
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void cobol2xml(InputStream cobolStream, OutputStream xmlStream)
			throws IOException, JAXBException, XMLStreamException;

	/**
	 * Convert Input Xml-Data to Cobol Data-File
	 * 
	 * @param xmlFileName Input Xml-File name 
	 * @param cobolFileName Ouput Cobol-Data File name
	 * 
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void xml2Cobol(String xmlFileName, String cobolFileName)
			throws IOException, JAXBException, XMLStreamException;

	/**
	 * Convert a Xml-Data in to a Cobol Data 
	 * @param xmlStream Input Xml Data
	 * @param cobolStream Output Cobol data
	 * 
	 * @throws IOException
	 * @throws JAXBException
	 * @throws XMLStreamException
	 */
	public void xml2Cobol(InputStream xmlStream, OutputStream cobolStream)
			throws IOException, JAXBException,
			XMLStreamException;

	/**
	 * 
	 * @return an IOBuilder related to the Cobol2Xml class
	 */
	public ISchemaIOBuilder asIOBuilder();





}
