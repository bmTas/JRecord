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

public interface ISetDropCopybookName {

	/**
	 * @param dropCopybookFromFieldNames the dropCopybookFromFieldNames to set
	 */
	public abstract void setDropCopybookFromFieldNames(
			boolean dropCopybookFromFieldNames);

	/**
	 * wether to keep the source cb2xml document
	 * 
	 * @param saveCb2xml wether to keep the source cb2xml document
	 */
	public abstract void setSaveCb2xmlDocument(boolean saveCb2xml);

	
	public abstract void setKeepFillers(boolean keepFiller);
}