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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.Icb2xmlLoadOptions;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

/**
 * Class To convert <i>Cobol Data Files</i> to/from <i>Xml Data files</i> using a cb2xml Xml-Schema,
 * This class defines a "Builder" interface for loading the schema-data.
 *  
 * @author Bruce Martin
 *
 */
public interface Icb2xml2Xml extends Icb2xmlLoadOptions {
	public static final String MAIN_XML_TAG = "CobolData";

	@Override public abstract Icb2xml2Xml setFileOrganization(int fileOrganization);

	@Override public abstract Icb2xml2Xml setSplitCopybook(int splitCopybook);


	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	@Override public abstract Icb2xml2Xml setFont(String font);

	@Override public abstract Icb2xml2Xml setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	@Override public abstract Icb2xml2Xml setRecordParent(String recordName, String parentName);

	@Override public abstract Icb2xml2Xml setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

	@Override public abstract Icb2xml2Xml setRecordDecider(RecordDecider recordDecider);


	/**
	 * Cobol is a column-sensitive language; Traditionally columns 1-5 are used for line-numbers (or version comment)
	 * and ignore everything after column 72. This parameter controls which part of the line to use. Supported values:<ul>
	 *   <li><b>Cb2xmlConstants.USE_STANDARD_COLUMNS</b> -  use columns 6-72 (normal format for mainframe copybooks), this is the default.
	 *   <li><b>Cb2xmlConstants.USE_COLS_6_TO_80</b> -  use columns 6-80
	 *   <li><b>Cb2xmlConstants.USE_LONG_LINE</b> -  use columns 6-10000
	 *   <li><b>Cb2xmlConstants.USE_PROPERTIES_FILE</b> -  columns are supplied in cb2xml.properties file.
	 * </ul>
	 * @param copybookFileFormat the copybookFileFormat to set
	 */
	public abstract Icb2xml2Xml setCopybookFileFormat(int copybookFileFormat);

	@Override public abstract Icb2xml2Xml setInitToSpaces(boolean initToSpaces);

	/**
	 * Setup check on wether to write Array Item.
	 * @param arrayName Array name
	 * @param check check to be performed; predefined checks include<ul>
	 * <li><b>ArrayElementChecks.INSTANCE.newSkipSpaces()</b> Skip array element if it is all spaces
	 * <li><b>ArrayElementChecks.INSTANCE.newStopAtSpaces()</b> Stop processin array elements if the current element spaces
	 * <li><b>ArrayElementChecks.INSTANCE.newSkipSpacesZeros()</b> Skip array element if it is all spaces/zeros
	 * <li><b>ArrayElementChecks.INSTANCE.newSkipLowValues()</b> Skip array element if it is low values
	 * <li><b>ArrayElementChecks.INSTANCE.newIndexCheck(arraySizeVariableName)</b> process array based on the array size
	 * <li>many others
	 * </ul>
	 * @return this Builder
	 */
	public abstract Icb2xml2Xml setArrayCheck(String arrayName, IArrayItemCheck check);

	/**
	 * Supply your own Xml input factory
	 * 
	 * @param xmlInputFactory Xml-input-factory to be used.
	 * 
	 * @return this for further updates
	 */
	public abstract Icb2xml2Xml setXmlInputFactory(XMLInputFactory xmlInputFactory);

	/**
	 * Supply your own XmlOutputFactory 
	 * 
	 * @param xmlOutputFactory Xml-Output-factory to use.
	 * @return this for further updates
	 */
	public abstract Icb2xml2Xml setXmlOutputFactory(XMLOutputFactory xmlOutputFactory);

	/**
	 * Set The Format  of Xml
	 * @param tagFormat How to format Cobol-names as Xml-Tags, valuies<ul>
	 * <li><b>Cbl2XmlValues.RO_LEAVE_ASIS</b> (Default) Keep the Cobol variable name
	 * <li><b>Cbl2XmlValues.RO_MINUS_TO_UNDERSCORE</b> Convert Minus (-) to underscore (_) in Cobol name.
	 * Cobol-Var-Name ==&gt; Cobol_Var_Name
	 * <li><b>Cbl2XmlValues.RO_CAMEL_CASE</b> Camel case conversion Cobol-Var-Name ==&gt; cobolVarName
	 * </ul>
	 * @return
	 */
	public abstract Icb2xml2Xml setTagFormat(int tagFormat);

	
	/**
	 * Normally the main element of the Xml is CobolData but you can
	 * set it to anything you want with this option.
	 * 
	 * @param xmlMainElement main tag name of the Xml
	 * 
	 * @return this item for "fluid" style assignment
	 */
	public Icb2xml2Xml setXmlMainElement(String xmlMainElement);
	
	@Override  public Icb2xml2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

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
	 * Set "formatField". This class will format the field-output prior to writing to a file.
	 * @param formatField new formatField
	 * @return
	 */
	public Icb2xml2Xml setFormatField(IFormatField formatField);


}
