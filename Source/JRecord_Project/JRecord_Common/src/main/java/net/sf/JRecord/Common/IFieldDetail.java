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

import net.sf.JRecord.Option.IOptionResult;
import net.sf.JRecord.Option.IOptionType;
import net.sf.JRecord.detailsBasic.CsvCharDetails;
import net.sf.JRecord.detailsBasic.IItemDetails;

/**
 * Description of one field in a Record (Line)
 *
 * @author Bruce Martin
 *
 */
public interface IFieldDetail {

	public abstract IFieldDetail setPosLen(final int pPosition, final int pLength);

	public abstract IFieldDetail setPosOnly(final int pPosition);

	public abstract int getDecimal();

	public abstract int getLen();

	public abstract String getName();
	
	public abstract String getLookupName();
	
	public abstract void setLookupName(String lookupName);

	public abstract int getPos();

	public abstract int getType();

	public abstract String getDescription();

	@Deprecated
	public abstract int getEnd();

	public abstract boolean isFixedFormat();

	public abstract String getFontName();

	public abstract int getFormat();

	public abstract String getParamater();

	public abstract CsvCharDetails getQuoteDefinition();

	public abstract AbstractRecord getRecord();

//	public abstract void setRecord(AbstractRecord record);

	public abstract void setNameType(String newName, int newType);

	public abstract Object getDefaultValue();

	public abstract void setDefaultValue(Object defaultValue);
	
	public abstract IOptionResult getOption(IOptionType type);

	public abstract int calculateActualPosition(AbstractIndexedLine line);

//	public abstract int calculateActualLength(AbstractIndexedLine line);
	
	public abstract int calculateActualEnd(AbstractIndexedLine line);

	public abstract IItemDetails getCobolItem();

}