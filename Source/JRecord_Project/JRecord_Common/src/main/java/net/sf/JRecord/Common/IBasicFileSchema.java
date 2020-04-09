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
      
package net.sf.JRecord.Common;

import net.sf.JRecord.detailsBasic.CsvCharDetails;

/**
 * <p>A minimal <i>File Schema</i> definition. It provides a
 * minimal set of definitions (like RecordLength & character=set)
 * for creating Readers / Writer.
 * <p>The schema's of JRecord / RecordEditor / ReCsvEditor / ProtoBufEditor
 * extend / implement this interface.
 * 
 * @author Bruce Martin
 *
 */
public interface IBasicFileSchema {

	/**
	 * Return the file structure
	 *
	 * @return file structure
	 */
	public abstract int getFileStructure();

	/**
	 * wether it is a binary record
	 *
	 * @return wether it is a binary record
	 */
	public abstract boolean isBinary();
	
	public abstract boolean useByteRecord();

//	public abstract boolean isBinCSV();

//	/**
//	 * Get the record Seperator bytes
//	 *
//	 * @return Record Seperator
//	 */
//	public abstract byte[] getRecordSep();


	/**
	 * Get the Canonical Name (ie Font name)
	 *
	 * @return Canonical Name (ie Font name)
	 */
	public abstract String getFontName();
//
//	/**
//	 * get the field delimiter
//	 * @return the field delimeter
//	 */
//	public abstract String getDelimiter();


	/**
	 * Get the field Delimiter
	 * @return field Delimiter
	 */
	public CsvCharDetails getDelimiterDetails();

	/**
	 * Get the Quote Details
	 * @return Quote Details
	 */
	public CsvCharDetails getQuoteDetails();


//	/**
//	 * get the field delimiter
//	 * @return the field delimeter
//	 */
//	public abstract byte[] getDelimiterBytes();

//	/**
//	 * Get Quote (for CSV files)
//	 * @return get the Quote character
//	 */
//	public abstract String getQuote();
	
	/**
	 * Get the maximum length of the Layout
	 *
	 * @return the maximum length
	 */
	public abstract int getMaximumRecordLength();

}
