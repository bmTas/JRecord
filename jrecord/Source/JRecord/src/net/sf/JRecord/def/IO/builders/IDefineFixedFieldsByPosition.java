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
 * This interface defines fixed width files using the position of this field & the next field.
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
 * @author Bruce Martin
 *
 */
public interface IDefineFixedFieldsByPosition {

	
	/**
	 * Add a field specifying the starting position of the field
	 * 
	 * @param name field name
	 * @param type Field type. Values include:<ul>
	 *  <li><b>Type.ftChar</b> - character field
	 *  <li><b>Type.ftNumLeftJustified</b> - left justified namber
	 *  <li><b>Type.ftNumRightJustified</b> - Right justified number
	 * </ul>
	 * @param pos starting position of the field
	 * @param decimal number of decimal places (fixed length numeric fields)
	 * 
	 * @return This schema builder so more fields can be added.
	 */
	public IDefineFixedFieldsByPosition addFieldByPosition(String name, int type, int pos, int decimal);
	
	/**
	 * Skip a field starting a a specified position
	 * 
	 * @param pos position of field to be skipped 
	 * 
	 * @return This schema builder so more fields can be added.
	 */
	public IDefineFixedFieldsByPosition skipFieldPosition(int pos);

	/**
	 * Marks the end of Field (or Column) Definition and returns to the FixedWidthIOBuilder
	 * 
	 * @param recordLength length of the record
	 * 
	 * @return IFixedWidthIOBuilder to create Readers / writers
	 */
	public IFixedWidthIOBuilder endOfRecord(int recordLength);
}
