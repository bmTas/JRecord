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

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

/**
 * This interface lets you define fixed width files by either there Length or position.
 * 
 * <pre>
 *     AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH
 *                         .newIOBuilder()
 *                             .defineFieldsByLength()
 *                                 .<b>addFieldByLength</b>("Sku"  , Type.ftChar,   8, 0)
 *                                 .<b>addFieldByLength</b>("Store", Type.ftNumRightJustified, 3, 0)
 *                                 .<b>addFieldByLength</b>("Date" , Type.ftNumRightJustified, 6, 0)
 *                                 .<b>addFieldByLength</b>("Dept" , Type.ftNumRightJustified, 3, 0)
 *                                 .<b>addFieldByLength</b>("Qty"  , Type.ftNumRightJustified, 2, 0)
 *                                 .<b>addFieldByLength</b>("Price", Type.ftNumRightJustified, 6, 2)
 *                             .<b>endOfRecord</b>()
 *                             .newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
 * </pre>
 * or
 * <pre>
 *            AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
 *                        .defineFieldsByPosition()
 *                            .<b>addFieldByPosition</b>("Sku"  , Type.ftChar             ,  1, 0)
 *                            .<b>addFieldByPosition</b>("Store", Type.ftNumRightJustified,  9, 0)
 *                            .<b>addFieldByPosition</b>("Date" , Type.ftNumRightJustified, 12, 0)
 *                            .<b>addFieldByPosition</b>("D<b><b>addFieldByPosition</b></b>ightJustified, 18, 0)
 *                            .<b>addFieldByPosition</b>("Qty"  , Type.ftNumRightJustified, 21, 0)
 *                            .<b>addFieldByPosition</b>("Price", Type.ftNumRightJustified, 23, 2)
 *                        .<b>endOfRecord</b>(29)
 *                        .newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
 * </pre>
 * 
 * 
 * @author Bruce Martin
 *
 */
public interface IFixedWidthIOBuilder extends IIOBuilder {

	/**
	 *
	 * <p>File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
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
     *      AbstractLineReader r = JRecordInterface1.FIXED_WIDTH
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
	 */
	public abstract IFixedWidthIOBuilder setFileOrganization(int fileOrganization);
	
	/**
	 * Set the font name
	 * 
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract IFixedWidthIOBuilder setFont(String font);

	

	/**
	 *
	 * Define record-selection criteria. For a single selection you would do
	 * 
	 * @param recordName name of the record 
	 * @param selectionCriteria selection-criteria
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
	 * </pre>
	 * 
	 *
	 */
	public abstract IFixedWidthIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);


	/**
	 * 	Start defing the fields using the position in the record The length is calculated
	 * as
	 * <pre>  
	 *       Length = Next_Field.position - current-field.position + 1
	 * </pre> 
	 *
	 * @return Class to define the fields using there position
	 */
	public abstract IDefineFixedFieldsByPosition defineFieldsByPosition();

	/**
	 * Start defining the fields
	 * @return class to define fields in the record.
	 */
	public abstract IDefineFixedFieldsByLength defineFieldsByLength();
}
