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

import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.detailsBasic.CsvCharDetails;

/**
 * Very Basic RecordLayout interface. It is used by the field Definitions to get
 * Data from the RecordDefinition that it belongs to
 *
 * @author Bruce Martin
 *
 */
public interface AbstractRecord {

	/**
	 * Get quote
	 * @return Get quote
	 */
	public abstract CsvCharDetails getQuoteDefinition();

//	public abstract String getQuoteUneditted();

	/**
	 * @return the parentRecordIndex
	 */
	public abstract int getParentRecordIndex();

	/**
	 * @return the recordStyle
	 */
	public abstract int getRecordStyle();

	/**
	 * @return the sourceIndex
	 */
	public abstract int getSourceIndex();

	/**
	 * Calculate the actual position (adjusting for any depending on values
	 * @param pos initial unadjusted position
	 * @return adjusted position
	 */
	public abstract int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos);
	
	
	/**
	 * Get a parser to split a line into seperate fields
	 * @return
	 */
	public abstract ICsvCharLineParser getCharParser();
}