/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
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
      
package net.sf.JRecord.CsvParser;

import net.sf.JRecord.detailsBasic.CsvCharDetails;

public interface ICsvDefinition {

	public int NORMAL_SPLIT = 1;
	public int SEP_FOR_EVERY_FIELD = 2;			// Delimiter for every field
	public int SEP_FOR_EVERY_FIELD_PLUS_END = 3;// Delimiter for every field + one at the end


	/**
	 * Get the field Delimiter
	 * @return field Delimiter
	 */
	public CsvCharDetails getDelimiterDetails();

	/**
	 * Get the Quote Details
	 * @return Quote Details
	 */
	public CsvCharDetails getQuoteDefinition();

	/**
	 * Get how the delimiter are organized (Normal, SEP_FOR_EVERY_FIELD, SEP_FOR_EVERY_FIELD_PLUS_END )
	 * @return Field Delimiter placement
	 */
	public int getDelimiterOrganisation();

	/**
	 * Get the number of fields
	 * @return number of fields
	 */
	public int getFieldCount();

	/**
	 * Get character set
	 * @return the character-set (font) of the Csv file.
	 */
	public String getFontName();

	/**
	 * Wether is is a single byte font being used
	 * @return Wether is is a single byte font being used
	 */
	public boolean isSingleByteFont();

	/**
	 * wether it has embedded newline characters
	 * @return the fields could have embedded new line characters
	 */
	public abstract boolean isEmbeddedNewLine();
}
