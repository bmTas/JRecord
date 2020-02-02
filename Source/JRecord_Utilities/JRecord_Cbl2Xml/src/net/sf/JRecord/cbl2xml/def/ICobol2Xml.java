/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
 *
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.cbl2xml.def;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.fieldNameConversion.IRenameField;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.fieldRename.IGetRecordFieldByName;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;


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
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */

	public abstract ICobol2Xml setDialect(int dialect);

	@Override public abstract ICobol2Xml setFont(String font);

	@Override public abstract ICobol2Xml setInitToSpaces(boolean initToSpaces);

	@Override public abstract ICobol2Xml setRecordSelection(String recordName, ExternalSelection selectionCriteria);
	
	@Override public abstract ICobol2Xml setRecordDecider(RecordDecider recordDecider);

	/**
	 * {@inheritDoc}
	 */
	@Override public abstract ICobol2Xml setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);


	@Override public abstract ICobol2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	@Override public abstract ICobol2Xml setArrayCheck(String arrayName, IArrayItemCheck check);

	@Override public abstract ICobol2Xml setXmlInputFactory(XMLInputFactory xmlInputFactory);
	
	@Override public abstract ICobol2Xml setXmlOutputFactory(XMLOutputFactory xmlOutputFactory);

	/**
	 * Set The Format  of Xml
	 * @param tagFormat How to format Cobol-names as Xml-Tags, valuies<ul>
	 * <li><b>IReformatFieldNames.RO_LEAVE_ASIS</b> (Default) Keep the Cobol variable name
	 * <li><b>IReformatFieldNames.RO_MINUS_TO_UNDERSCORE</b> Convert Minus (-) to underscore (_) in Cobol name.
	 * Cobol-Var-Name ==&gt; Cobol_Var_Name
	 * <li><b>IReformatFieldNames.RO_CAMEL_CASE</b> Camel case conversion Cobol-Var-Name ==&gt; cobolVarName
	 * </ul>
	 * @return this 
	 */
	public abstract ICobol2Xml setTagFormat(int tagFormat);
	
	/**
	 * Define the Field Rename option
	 * @param renameFieldClass class to rename cobol fields
	 * @return this
	 */
	public abstract ICobol2Xml setRenameFieldClass(IRenameField renameFieldClass);


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
	
	public abstract ICobol2Xml setFormatField(IFormatField formatField);

	public abstract ICobol2Xml setFieldNameLookup(IGetRecordFieldByName fieldNameLookup);

//	/**
//	 * Convert Cobol Data File to Xml file
//	 * 
//	 * @param cobolFileName input Cobol-Data file name
//	 * @param xmlFileName output Xml-Data file name
//	 * 
//	 * @throws IOException 
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void cobol2xml(String cobolFileName, String xmlFileName)  
//			throws  IOException, XMLStreamException;
//	
//	/**
//	 * Convert Cobol Data File to Xml file
//	 * 
//	 * @param cobolStream
//	 * @param xmlStream
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void cobol2xml(InputStream cobolStream, OutputStream xmlStream)
//			throws IOException, XMLStreamException;
//
//	/**
//	 * Convert Input Xml-Data to Cobol Data-File
//	 * 
//	 * @param xmlFileName Input Xml-File name 
//	 * @param cobolFileName Ouput Cobol-Data File name
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void xml2Cobol(String xmlFileName, String cobolFileName)
//			throws IOException, XMLStreamException;
//
//	/**
//	 * Convert a Xml-Data in to a Cobol Data 
//	 * @param xmlStream Input Xml Data
//	 * @param cobolStream Output Cobol data
//	 * 
//	 * @throws IOException
//	 * @throws JAXBException
//	 * @throws XMLStreamException
//	 */
//	public void xml2Cobol(InputStream xmlStream, OutputStream cobolStream)
//			throws IOException,
//			XMLStreamException;

	/**
	 * 
	 * @return an IOBuilder related to the Cobol2Xml class
	 */
	public ISchemaIOBuilder asIOBuilder();





}
