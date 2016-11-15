/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.schema;


import java.io.IOException;

import javax.xml.bind.JAXBException;

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlLoadOptions;


/**
 * Class To create and <i>Extended Schema</i> class {@link CobolSchemaDetails}.
 * This <i>Extended Schema</i> class holds:<ul>
 *  <li>JRecord LayoutDetail - Standard JRecord-File-Schema
 *  <li>Cobol-Item-Tree for each Record with JRecord FieldDetails / ArrayDetails
 *  <li>JRecord-IOBuilder
 * </ul>
 *  
 * @author Bruce Martin
 *
 */
public interface ICobolSchemaReader  extends  Icb2xmlLoadOptions  {

	@Override public abstract ICobolSchemaReader setFileOrganization(int fileOrganization);

	@Override public abstract ICobolSchemaReader setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */
	public abstract ICobolSchemaReader setDialect(int dialect);

	@Override public abstract ICobolSchemaReader setFont(String font);

	@Override public abstract ICobolSchemaReader setInitToSpaces(boolean initToSpaces);

	@Override public abstract ICobolSchemaReader setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	@Override public abstract ICobolSchemaReader setRecordDecider(RecordDecider recordDecider);
	
	/**
	 * {@inheritDoc}
	 */
	@Override public abstract ICobolSchemaReader setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

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
	public abstract ICobolSchemaReader setCopybookFileFormat(int copybookFileFormat);

	@Override public abstract ICobolSchemaReader setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * Set The Format  of Output Tag
	 * @param tagFormat How to reformat Cobol-names, valuies<ul>
	 * <li><b>IReformatFieldNames.RO_LEAVE_ASIS</b> (Default) Keep the Cobol variable name
	 * <li><b>IReformatFieldNames.RO_MINUS_TO_UNDERSCORE</b> Convert Minus (-) to underscore (_) in Cobol name.
	 * Cobol-Var-Name ==&gt; Cobol_Var_Name
	 * <li><b>IReformatFieldNames.RO_CAMEL_CASE</b> Camel case conversion Cobol-Var-Name ==&gt; cobolVarName
	 * </ul>
	 * @return this 
	 */
	public abstract ICobolSchemaReader setTagFormat(int tagFormat);

	
	/**
	 * Define the parent record.
	 * 
	 * @param recordName record name
	 * @param parentName name of the parent record.
	 * 
	 * @return this
	 */
	public abstract ICobolSchemaReader setRecordParent(String recordName, String parentName);
	


	/**
	 * 
	 * @return an IOBuilder related to the Cobol2Xml class
	 */
	public abstract ISchemaIOBuilder asIOBuilder();

	/**
	 * @return Extended Schema details
	 * 
	 */
	public abstract CobolSchemaDetails getCobolSchemaDetails() throws IOException,
			JAXBException;



}
