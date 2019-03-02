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

package net.sf.JRecord.Details;

/**
 * Create a XmlLine from a String / Byte array
 * 
 * @author Bruce Martin
 *
 */
public class XmlLineProvider implements LineProvider {

	/**
	 * {@link LineProvider#getLine(LayoutDetail)}
	 */
	public AbstractLine getLine(LayoutDetail recordDescription) {
		return new XmlLine(recordDescription, -1);
	}

	/**
	 * @see LineProvider#getLine(LayoutDetail, String)
	 */
	public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {
		return getLine(recordDescription);
	}

	/**
	 * @see LineProvider#getLine(LayoutDetail, byte[])
	 */
	public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
		return getLine(recordDescription);
	}

}
