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
 * Interface for defining Fixed Width files using the Field Length:
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
 * 
 * @author Bruce Martin
 *
 */
public interface IDefineFixedFieldsByLength {

	/**
	 * Add a field specifying the field length
	 * @param name field name
	 * @param type Field type. Values include:<ul>
	 *  <li>Type.ftChar - character field
	 *  <li>Type.ftNumLeftJustified - left justified namber
	 *  <li>Type.ftNumRightJustified - Right justified number
	 * </ul>
	 * @param length field Length
	 * @param decimal number of decimal places
	 * @return IOBuilder
	 * 
	 *  <pre>
	 *                             .defineFieldsByLength()
	 *                                 .<b>addFieldByLength</b>("Sku"  , Type.ftChar,   8, 0)
	 *                                 .<b>addFieldByLength</b>("Store", Type.ftNumRightJustified, 3, 0)
	 *                                 .<b>addFieldByLength</b>("Date" , Type.ftNumRightJustified, 6, 0)
	 *                                 .<b>addFieldByLength</b>("Dept" , Type.ftNumRightJustified, 3, 0)
	 *                                 .<b>addFieldByLength</b>("Qty"  , Type.ftNumRightJustified, 2, 0)
	 *                                 .<b>addFieldByLength</b>("Price", Type.ftNumRightJustified, 6, 2)
     *                             .<b>endOfRecord</b>()
     * </pre>
	 */
	public IDefineFixedFieldsByLength addFieldByLength(String name, int type, int length, int decimal);
	
	/**
	 * Skip a specified number of bytes in the record.
	 * @param numberOfBytes number of bytes to be skipped
	 * @return this Builder so the user can define more fields
	 */
	public IDefineFixedFieldsByLength skipBytes(int numberOfBytes);
	
	/**
	 * Marks the end of Field (or Column) Definition and returns to the FixedWidthIOBuilder
	 * 
	 * @return IFixedWidthIOBuilder to create Readers / writers
	 */
	public IFixedWidthIOBuilder endOfRecord();
}
