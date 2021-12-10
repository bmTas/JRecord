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
      
package net.sf.JRecord.cgen.def;

import net.sf.JRecord.detailsBasic.CsvCharDetails;

/**
 * This is Interface that is implemented by LayoutDetails classes in both JRecord and the RecordEditor.
 * It allows CodeGen to be use with both systems
 * 
 * @author Bruce Martin
 *
 */
public interface ILayoutDetails4gen {

	/**
	 * Get The File Structure Code (IO Type)
	 * @returnFile Structure Code
	 */
	public abstract int getFileStructure();

	/**
	 * Get the Schema (Layout) Type Code
	 * @return Schema (Layout) Type Code
	 */
	public abstract int getLayoutType();
	
	/**
	 * Get the Schema (Layout) name
	 * @return Schema (Layout) name
	 */
	public abstract String getLayoutName();
	
	/**
	 * Get the number of records  in the Schema
	 * @return number of records
	 */
	public abstract int getRecordCount();
	
	/**
	 * Get a specified record
	 * @param recordIndex  which record do you want
	 * @return requested record
	 */
	public abstract IRecordDetail4gen getRecord(int recordIndex);

	/**
	 * @return wether it is a Csv Schema (Layout) or not ??
	 */
	public abstract boolean isCsvLayout();

	/**
	 * Get the Field delimiter (only available for Csv Files).
	 * @return
	 */
	public abstract CsvCharDetails getDelimiterDetails();
	
	/**
	 * 
	 * @return wether the Schema describe a binary or Text file
	 */
	public boolean isBinary();
	
	/**
	 * 
	 * @return The font-name (encoding)
	 */
	public String getFontName();
	
	/**
	 * get the maximum possible record length
	 * @return maximum possible record length
	 */
	public int getMaximumRecordLength();
}
