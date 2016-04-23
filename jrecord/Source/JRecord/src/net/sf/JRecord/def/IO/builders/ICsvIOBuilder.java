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

package net.sf.JRecord.def.IO.builders;

/**
 * <p>This interface is for creating CSV-Readers and writers</p>
 * 
 * <pre>
 * You can create a Csv-Reader by:
 * 
 *     AbstractLineReader reader = JRecordInterface1.CSV
 *             .newIOBuilder("    ", "\"")
 *                     .newReader(salesFile);
 *                     
 * and a Csv Writer by:
 * 
 *     ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV
 *             .newIOBuilder(";", "\"")
 *                     .defineFields()
 *                         .addCsvField(FLD_SKU,   Type.ftChar, 0)
 *                         .addCsvField(FLD_STORE, Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_DATE,  Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_QTY,   Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_PRICE, Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_GST,   Type.ftNumAnyDecimal, 0)
 *                     .endOfRecord();
 *   </pre>
 * 
 * @author Bruce Martin
 *
 */
public interface ICsvIOBuilder extends IIOBuilder {

	/**
	 * File Organization or File Structure, For Csv files, the main options are:<ul>
	 *   <li><b>Constants.IO_UNICODE_NAME_1ST_LINE</b> - Standard Csv File <b>with names on the first line</b> of the file.
	 *   It supports either Unicode or single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.
	 *   <li><b>Constants.IO_UNICODE_TEXT</b> - Standard Csv File <b><font color="blue">with out</font> names on the first line</b> of the file.
	 *   It supports either Unicode or single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.
	 *   <li><b>Constants.IO_UNICODE_CSV_NAME_1ST_LINE</b> - Standard Csv File <b>with names on the first line</b> of the file.
	 *   It supports either Unicode or single byte character-sets. Embedded <b>\n <font color="blue"></b>are</font> supported.
	 *   <li><b>Constants.IO_UNICODE_CSV</b> - Standard Csv File <b><font color="blue">with out</font> names on the first line</b> of the file.
	 *   It supports either Unicode or single byte character-sets. Embedded <b>\n <font color="blue"></b>are</font> supported.
	 *   <li><b>Constants.IO_NAME_1ST_LINE</b> - Standard <b>Single Byte</b> (ascii ?) Csv File <b>with names on the first line</b> of the file.
	 *   It supports single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.
	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard <b>Single Byte</b> (ascii ?) Csv File <b><font color="blue">with out</font> names on the first line</b> of the file.
	 *   It supports single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.<p>&nbsp;</p>
	 *   <li><b>Constants.IO_BIN_NAME_1ST_LINE</b> - Standard <b>Single Byte</b> (ascii ?) Csv File <b>with names on the first line</b> of the file.
	 *   The <b><font color="blue">field separator</font>/b> is specified as a <b><font color="blue">hex</font>/b> character (e.g. x'00').
	 *   It supports single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.
	 *   <li><b>Constants.IO_BIN_TEXT</b> - Standard <b>Single Byte</b> (ascii ?) Csv File <b><font color="blue">with out</font> names on the first line</b> of the file.
	 *   The <b><font color="blue">field separator</font>/b> is specified as a <b><font color="blue">hex</font>/b> character (e.g. x'00').
	 *   It supports single byte character-sets. Embedded <b>\n</b> are <b>not</b> supported.<br/>
	 * </ul>
	 * 
	 * @param fileOrganization File Organization (or File Structure)
	 */
	public abstract ICsvIOBuilder setFileOrganization(int fileOrganization);
	


	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract ICsvIOBuilder setFont(String font);

	/**
	 * Set the type of parser to use
	 * @param csvParser type of parser to use. Options include<ul>
	 * <li>ParserManager.BASIC_CSV_PARSER,                    Parse Csv - when a field starts with " look for "<FieldSeparator> or "<eol> 
     * <li>ParserManager.STANDARD_CSV_PARSER,                 Parse CSV matching Quotes
     * <li>ParserManager.DB_CSV_PARSER,                       Standard Parser, add Quotes to all Char fields
     * <li>ParserManager.BASIC_QUOTED_COL_NAME_CSV_PARSER,    Basic Parser, Field (Column) names in Quotes
     * <li>ParserManager.STANDARD_QUOTED_COL_NAME_CSV_PARSER, Standard Parser, Field (Column) names in Quotes
     * <li>ParserManager.DB_QUOTED_COL_NAME_CSV_PARSER,       Standard Parser, Char fields in Quotes,  Field (Column) names in Quotes
     * <li>ParserManager.BASIC_ENSURE_CORRECT_NO_FIELDS,      Basic Parser, Field Separator for all fields
     * <li>ParserManager.BASIC_ENSURE_CORRECT_NO_FIELDS_P1,   Basic Parser, Field Separator for all fields + extra Separator at the End-of-Line
     * </ul>
	 * @return
	 */
	public abstract ICsvIOBuilder setParser(int csvParser);
//
//	/**
//	 * Define record-selection criteria. For a single selection you would do
//	 * 
//	 * @param recordName name of the record 
//	 * @param selectionCriteria selection-criteria
//	 * 
//	 * @return updated IOBuilder
//	 * 
//	 * <pre>
//	 * Usage:
//	 * 
//	 *   IOBldr.setRecordSelection("Header-Record", new FieldSelection("Record-Type", "H"));
//	 * </pre>   
//	 *  or if you want to use or's / and's  
//	 * <pre>  
//	 *   IOBldr.setRecordSelection(
//     *          "Trailer-Record",
//     *          ExternalGroupSelection.newOr(
//     *                  new ExternalFieldSelection("Record-Type", "D"),
//     *                  new ExternalFieldSelection("Record-Type", "E"),
//     *                  new ExternalFieldSelection("Record-Type", "F")
//     * 	 ));
//     * 
//     * This is basically the following expression:
//     *    
//     *         Record-Type = "D"
//     *      or Record-Type = "E"
//     *      or Record-Type = "F"
//     *      
//     * and more complicated boolean logic is possible:
//     * 
//     * 
//     * 	 IOBldr.setRecordSelection(
//     *          "Trailer-Record",
//     *          ExternalGroupSelection.newAnd(
//     *               new ExternalFieldSelection("Record-Type-1", "D"),
//     *               ExternalGroupSelection.newOr(
//     *                    new ExternalFieldSelection("Record-Type-2", "D"),
//     *                    new ExternalFieldSelection("Record-Type-2", "E"),
//     *                    new ExternalFieldSelection("Record-Type-2", "F")
//     *               )
//     *          ));
//     *          
//     *   which is
//     *   
//     *             Record-Type-1 = "D"
//     *       and ( Record-Type-2 = "D"
//     *          or Record-Type-2 = "E"
//     *          or Record-Type-2 = "F" )     
//	 * </pre>
//	 * 
//	 *
//	 */
//	public abstract ICsvIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);


	/*
	 * 	 * <b><font color="red">Warning</font> This option has not been tested yet !!!</b>
	 *

	 */
	public abstract IDefineCsvFields defineFields();


	/**
	 * Set the Csv delimiter or field separator
	 * @param val new field delimiter or Field separator
	 * @return this ICsvIOBuilder
	 */
	public abstract ICsvIOBuilder setDelimiter(String val);


	/**
	 * Set the Quote character to be used 
	 * @param val quote value
	 * @return this ICsvIOBuilder
	 */
	public abstract ICsvIOBuilder setQuote(String val);
}
