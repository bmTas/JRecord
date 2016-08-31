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

package net.sf.JRecord.IO.builders;

import java.io.IOException;

import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public interface ICreateExternal {

	/**
	 * Create the requested External Record
	 * @param splitCopybook wether to split the copybook
	 * @return requested External Record
	 * 
	 * @throws IOException
	 */
	public abstract ExternalRecord createExternalRecord()
			throws Exception;

	public abstract void setSplitCopybook(int splitCopybook);
	
	public abstract void setRecordSelection(ExternalSelection recordSelection);
	
	public abstract void setStartPosition(IStartingPosition startPosition);
	
	public abstract String getRecordName();
	
	public abstract ExternalRecord getLastExternalRecord();
	
	public abstract void clearLastRecord();
}