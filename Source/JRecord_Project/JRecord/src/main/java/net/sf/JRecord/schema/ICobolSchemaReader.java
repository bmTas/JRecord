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

	/**
	 * File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
	 * The main options are:<ul>
	 *   <li><b>Constants.IO_DEFAULT</b> - JRecord will decide the actual method based on other values (The Default Value).
	 *   It is generally better to explicitly set the File-Organisation (or file-Structure).
	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard Windows/*nix/Mac text file using  \n, \n\r etc as a record (or line) delimiter.
	 *   <li><b>Constants.IO_UNICODE_TEXT</b> - Standard Windows/*nix/Mac Unicode / double byte text file using  \n, \n\r etc 
	 *   as a record (or line) delimiter. It ensures record are stored in character format (instead of bytes).
	 *   <li><b>Constants.IO_FIXED_LENGTH</b> - Every Record (or line) is a standard Fixed length based on the Maximum
	 *   schema (LayoutDetail) record length.
	 *   <li><b>Constants.IO_FIXED_LENGTH_CHAR</b> - Fixed length character file (typically used for Fixed-Length unicode files).
	 *   <li><b>Constants.IO_VB</b> - Mainframe VB (Variable Record length file). Records consist of a Record-Length followed by the Record-Data.
	 *   <li><b>Constants.IO_VB_DUMP</b> - Raw Block format of a Mainframe-VB file. You get this format  if you specify RECFM=U when reading it on the mainframe.
	 *   <li><b>Constants.IO_VB_GNU_COBOL</b> - GNU (open-Cobol) VB format.
	 * </ul>
	 *<pre>
     *<b>Example:</b> 
	 *      {@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *}</pre> 
     *
     *
     *<pre>
	 *     <b>Variable Length</b> where the length is before the Record Data (IO_VB*):
	 *     
	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
	 *           
	 *          Record Record Data  	         
	 *          Length ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
     *              11 = Record 1>
     *              21 = Record 2 --------->
     *              61 = Record 3 ------------------------------------------------->
     *              25 = Record 4 ------------->
	 *           
	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length (IO_FIXED_LENGTH and IO_FIXED_LENGTH_CHAR:
	 *     
	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
	 *          
	 *          Fixed length file (Record Length = 15):
	 *          
	 *          ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
	 *          = Record 1 ---&gt;= Record 2 ---&gt;= Record 3 ---&gt;= Record 4 ---&gt;= Record 5 ---&gt;
	 *          
	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
	 * </pre>
	 * 
	 * @param fileOrganization File Organization (or File Structure)
	public abstract ICobolSchemaReader setFileOrganization(int fileOrganization);

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

	public abstract ICobolSchemaReader setFont(String font);

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
	public abstract CobolSchemaDetails getCobolSchemaDetails() throws IOException;


}
