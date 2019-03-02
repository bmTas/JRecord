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

package net.sf.JRecord.External;


public interface IFixedWidthSchemaBuilder extends IBasicSchemaBuilder {

	/**
	 * Add a field to the Record using the Field position and calculating lengths.
	 * This is normally the last field defined
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param pos Fields position in the record
	 * @param length Field length
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	public IFixedWidthSchemaBuilder addFieldByPosition(String name, int type, int pos, int length, int decimal);
	
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
	 * @return this schema builder (for further updates)
	 */
	public IFixedWidthSchemaBuilder addFieldByLength(String name, int type, int length, int decimal);
	
	/**
	 * Skip a specified number of bytes in the record.
	 * @param numberOfBytes number of bytes to be skipped
	 * @return this schema builder (for further updates)
	 */
	public IFixedWidthSchemaBuilder skipBytes(int numberOfBytes);

	public IFixedWidthSchemaBuilder addField(String name, int type, int pos, int length, int decimal);

	public IFixedWidthSchemaBuilder addFieldByPosition(String name, int type, int pos,
			int decimal);

	public IFixedWidthSchemaBuilder skipFieldPosition(int pos);
	
}
