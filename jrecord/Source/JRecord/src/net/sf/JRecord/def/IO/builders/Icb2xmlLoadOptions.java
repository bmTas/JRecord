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

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;



/**
 * This interface will not be directly implemented (or probably used by users).
 * It defines the options to load a Cobol Copybook with there descriptions.
 * It allows these descriptions to be held in one place. Its purpose is to hold
 * common method documentation.
 * 
 * It will be referenced (and overriden) by CobolIOBuilder and Cobol <==> Xml
 * builder classes (+ possibly future Cobol <==> JSon builder classes). 
 *
 * @author Bruce Martin
 *
 */
public interface Icb2xmlLoadOptions {

//	/**
//	 * File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
//	 * The main options are:<ul>
//	 *   <li><b>Constants.IO_DEFAULT</b> - JRecord will decide the actual method based on other values (The Default Value).
//	 *   It is generally better to explicitly set the File-Organisation (or file-Structure).
//	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard Windows/*nix/Mac text file using  \n, \n\r etc as a record (or line) delimiter.
//	 *   <li><b>Constants.IO_UNICODE_TEXT</b> - Standard Windows/*nix/Mac Unicode / double byte text file using  \n, \n\r etc 
//	 *   as a record (or line) delimiter. It ensures record are stored in character format (instead of bytes).
//	 *   <li><b>Constants.IO_FIXED_LENGTH</b> - Every Record (or line) is a standard Fixed length based on the Maximum
//	 *   schema (LayoutDetail) record length.
//	 *   <li><b>Constants.IO_FIXED_LENGTH_CHAR</b> - Fixed length character file (typically used for Fixed-Length unicode files).
//	 *   <li><b>Constants.IO_VB</b> - Mainframe VB (Variable Record length file). Records consist of a Record-Length followed by the Record-Data.
//	 *   <li><b>Constants.IO_VB_DUMP</b> - Raw Block format of a Mainframe-VB file. You get this format  if you specify RECFM=U when reading it on the mainframe.
//	 *   <li><b>Constants.IO_VB_GNU_COBOL</b> - GNU (open-Cobol) VB format.
//	 * </ul>
//	 *<pre>
//     *<b>Example:</b> 
//	 *      {@code
//     *      AbstractLineReader r = JRecordInterface1.COBOL
//     *              .newIOBuilder("file-name")
//     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
//     *}</pre> 
//     *
//     *
//     *<pre>
//	 *     <b>Variable Length</b> where the length is before the Record Data (IO_VB*):
//	 *     
//	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
//	 *           
//	 *          Record Record Data  	         
//	 *          Length ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
//     *              11 = Record 1>
//     *              21 = Record 2 --------->
//     *              61 = Record 3 ------------------------------------------------->
//     *              25 = Record 4 ------------->
//	 *           
//	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length (IO_FIXED_LENGTH and IO_FIXED_LENGTH_CHAR:
//	 *     
//	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
//	 *          
//	 *          Fixed length file (Record Length = 15):
//	 *          
//	 *          ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
//	 *          = Record 1 ---&gt;= Record 2 ---&gt;= Record 3 ---&gt;= Record 4 ---&gt;= Record 5 ---&gt;
//	 *          
//	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
//	 * </pre>
//	 * 
//	 * @param fileOrganization File Organization (or File Structure)
//	 */
//	public abstract Icb2xmlLoadOptions setFileOrganization(int fileOrganization);

	/**
	 * @param splitCopybook Wether to Split the Copybook or not. Options are:<ul>
	 *    <li><b>CopybookLoader.SPLIT_NONE</b> - No Split
	 *    <li><b>CopybookLoader.SPLIT_REDEFINE</b> - Split on highest-Level redefines
	 *    <li><b>CopybookLoader.SPLIT_01_LEVEL</b> - Split on 01 levels
	 *    <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> - Split on the highest group level
	 *    in the copybook.
	 * </ul>
	 * <p>Most of the time it does not matter what this parameter is set to, exceptions  
	 * include copybook where there are multiple-records at a group level > 01 (use SPLIT_HIGHEST_REPEATING):
	 * <pre>
	 * 
	 *         05  Header-Record.
	 *             ....
	 *         05  Detail-Record.
	 *             ....
	 *         05  Trailer-Record.
	 *             ....
	 * </pre>
	 * 
	 * <li><b>CopybookLoader.SPLIT_NONE</b> where Records are redefined:
	 * <pre>
	 *      01 Input-Record.
	 *         05 Field-1                 pic x(..).
	 *         05 Field-2                 pic s9(4) comp.
	 *           ...
	 *         05 Field-n                 pic x(..).
	 * </pre>
	 *         
	 * <li><b>CopybookLoader.SPLIT_REDEFINE</b> for simple single record copybooks
	 * <pre>
	 *  
     *   01  Output-Record.
     *       05 Common-Record-Header.
     *          10 Record-type
     *             ...
     *       05 Record-1.
     *          .....
     *       05 Record-2 redefines Record-1.
     *          .....
     *       05 Record-3 redefines Record-1.
     *          .....
	 * </pre>
	 * <li><b>CopybookLoader.SPLIT_01_LEVEL</b> where Records are redefined:
	 * <pre>
	 * </pre>
	 *  01  Record-1.
     *      05 Record-Type
     *         ....
     *  01  Record-2.
     *      05 Record-Type
     *         ....
     *  01  Record-2.
     *      05 Record-Type
     *       ....
	 * </pre>
	 * <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> where Records are redefined:
	 * <pre>
	 * 
	 *         05  Header-Record.
	 *             ....
	 *         05  Detail-Record.
	 *             ....
	 *         05  Trailer-Record.
	 *             ....
	 * </pre>
	 * </ul> 
	 * 
	 * </pre>
	 */
	public abstract Icb2xmlLoadOptions setSplitCopybook(int splitCopybook);


//	/**
//	 * set the font (character set) of the file.
//	 * 
//	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
//	 */
//	public abstract Icb2xmlLoadOptions setFont(String font);
//
	

	/**
	 * Define record-selection criteria. For a single selection you would do
	 * 
	 * @param recordName name of the record 
	 * @param selectionCriteria selection-criteria. Typically you would
	 * use  new ExternalFieldSelection("Record-Type", "D") or similar
	 * 
	 * @return updated IOBuilder
	 * 
	 * <pre>
	 * Usage:
	 * 
	 *   IOBldr.setRecordSelection("Header-Record", new FieldSelection("Record-Type", "H"));
	 * </pre>   
	 *  or if you want to use or's / and's  
	 * <pre>  
	 *   IOBldr.setRecordSelection(
     *          "Trailer-Record",
     *          ExternalGroupSelection.newOr(
     *                  new ExternalFieldSelection("Record-Type", "D"),
     *                  new ExternalFieldSelection("Record-Type", "E"),
     *                  new ExternalFieldSelection("Record-Type", "F")
     * 	 ));
     * 
     * This is basically the following expression:
     *    
     *         Record-Type = "D"
     *      or Record-Type = "E"
     *      or Record-Type = "F"
     *      
     * and more complicated boolean logic is possible:
     * 
     * 
     * 	 IOBldr.setRecordSelection(
     *          "Trailer-Record",
     *          ExternalGroupSelection.newAnd(
     *               new ExternalFieldSelection("Record-Type-1", "D"),
     *               ExternalGroupSelection.newOr(
     *                    new ExternalFieldSelection("Record-Type-2", "D"),
     *                    new ExternalFieldSelection("Record-Type-2", "E"),
     *                    new ExternalFieldSelection("Record-Type-2", "F")
     *               )
     *          ));
     *          
     *   which is
     *   
     *             Record-Type-1 = "D"
     *       and ( Record-Type-2 = "D"
     *          or Record-Type-2 = "E"
     *          or Record-Type-2 = "F" )     
     *          
     *      * and more complicated boolean logic is possible:
     * 
     * 
     * 	 IOBldr.setRecordSelection(
     *          "Trailer-Record",
     *          ExternalGroupSelection.newAnd(
     *               new ExternalFieldSelection("Record-Type-1", "D"),
     *               ExternalGroupSelection.newOr(
     *                    new ExternalFieldSelection("Record-Type-2", "D"),
     *                    new ExternalFieldSelection("Record-Type-2", "E"),
     *                    new ExternalFieldSelection("Record-Type-2", "F")
     *               )
     *          ));
     *          
     *   which is
     *   
     *             Record-Type-1 = "D"
     *       and ( Record-Type-2 = "D"
     *          or Record-Type-2 = "E"
     *          or Record-Type-2 = "F" )     
         
	 * </pre>
	 * 
	 *
	 */
	public abstract Icb2xmlLoadOptions setRecordSelection(String recordName, ExternalSelection selectionCriteria);
	
	/**
	 * Set the Record Length for a fixed width file. You only need to 
	 * do this when the RecordLength is different to the longest Record
	 * in the Cobol copybook
	 * 
	 * @param recordLength Record Length of the file
	 * 
	 * @return this item for more updates
	 */
	public abstract Icb2xmlLoadOptions setRecordLength(int recordLength);

	/**
	 * Cobol Copybooks do not define how to determine which record is which
	 * in a way that a package like JRecord can determine. In some cases
	 * JRecord needs to know which record is which. You can use a RecordDecider 
	 * to tell JRecord which record to use.
	 * 
	 * <pre>
	 *      05  Record-Type                 pic x.
	 *          88 Header-Record-Type    'H'.
	 *          88 Detail-record-Type    'D'.
	 *           ...  
	 *      05  Header-Record.
	 *      
	 *      05  Detail-Record redefines Header-Record.
	 * </pre>
	 * 
	 * And in JRecord
	 * 
	 * <pre>
	 *     RecordDecider rd = JRecordInterface1.RECORD_DECIDER_BUILDER
	 *                              .singleFieldDeciderBuilder("Record-Type", true)
	 *                                  .addRecord("H", "Header-Record")
	 *                                  .addRecord("D", "Detail-Record")
	 *                              .build();
	 *     ioBuilder
	 *             .setRecordDecider(rd) ...
	 * </pre>
	 * @param recordDecider a Record-Decider to be used
	 * 
	 * @return this IOBuilder for future updates
	 */
	public abstract Icb2xmlLoadOptions setRecordDecider(RecordDecider recordDecider);
	
	/**
	 * Define the parent record.
	 * 
	 * @param recordName record name
	 * @param parentName name of the parent record.
	 * 
	 * @return this
	 */
	public abstract Icb2xmlLoadOptions setRecordParent(String recordName, String parentName);

//	/**
//	 * Cobol is a column-sensitive language; Traditionally columns 1-5 are used for line-numbers (or version comment)
//	 * and ignore everything after column 72. This parameter controls which part of the line to use. Supported values:<ul>
//	 *   <li><b>Cb2xmlConstants.USE_STANDARD_COLUMNS</b> -  use columns 6-72 (normal format for mainframe copybooks), this is the default.
//	 *   <li><b>Cb2xmlConstants.USE_COLS_6_TO_80</b> -  use columns 6-80
//	 *   <li><b>Cb2xmlConstants.USE_LONG_LINE</b> -  use columns 6-10000
//	 *   <li><b>Cb2xmlConstants.USE_PROPERTIES_FILE</b> -  columns are supplied in cb2xml.properties file.
//	 * </ul>
//	 * @param copybookFileFormat the copybookFileFormat to set
//	 */
//	public abstract Icb2xmlLoadOptions setCopybookFileFormat(int copybookFileFormat);


	/**
	 * whether to drop the copybook name from the start of the Field names. On the 
	 * mainframe it is quite common to start (or end) a field name with the copybook name. This parameter
	 * controls wether the this copybook name should be dropped or kept in JRecord.
	 * <pre>
	 * for copybook DTAR030:
	 * 
	 *     05 DTAR030.
	 *        10 DTAR030-Product-Code            pic 9(8).
	 *        10 DTAR030-Location                pic 9(4).
	 *        05 DTAR030-Quantity                pic s9(6) comp-3.
	 *        
	 *  this option lets you remove/keep the copybook name at the start of the field name.
	 * </pre>
	 * 
	 * @param dropCopybookNameFromFields drop the copybook name from the start of the Field names ?
	 * In the above example if<ul>
	 *   <li><b>true</b>  - DTAR030-Product-Code is converted to Product-Code.
	 *   <li><b>false</b> - DTAR030-Product-Code remains unchanged.
	 * </ul>
	 * <br><b>Note: </b> The default is <b>false</b>
	 */
	public abstract Icb2xmlLoadOptions setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * Set this as a position record (i.e. first or last record in the file)
	 * 
	 * @param recordName Name of Record to be updated 
	 * @param positionOption position type; options are <ul>
	 *     <li>Options.RP_FIRST_RECORD_IN_FILE
	 *     <li>Options.RP_MIDDLE_RECORDS
	 *     <li>Options.RP_LAST_RECORD_IN_FILE
	 * </ul>
	 * @return Builder for further updates
	 */
	public abstract Icb2xmlLoadOptions setRecordPositionCode(
			String recordName,
			IRecordPositionOption positionOption);
	
	/**
	 * Whether to initialize byte based records to spaces
	 * @param initToSpaces initialize byte records to spaces.
	 * The default is now:<ul>
	 *  <li>Binary files - initialize to hex zero's
	 *  <li>Text file - initialize to spaces.
	 * </ul> 
	 * 
	 */
	public abstract Icb2xmlLoadOptions setInitToSpaces(boolean initToSpaces);
}