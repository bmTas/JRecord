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

import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.ISetDropCopybookName;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public abstract class CreateExternalBase {
	final IGetLoader parent;

	int splitCopybook = CopybookLoader.SPLIT_NONE;
	private ExternalSelection recordSelection = null;

	protected CreateExternalBase(IGetLoader parent) {
		super();
		this.parent = parent;
	}

	
	/**
	 * @param split the split to set
	 */
	public final void setSplitCopybook(int split) {
		this.splitCopybook = split;
		doCheck();
	}
	
	/**
	 * @param recordSelection the recordSelection to set
	 */
	public final void setRecordSelection(ExternalSelection recordSelection) {
		this.recordSelection = recordSelection;
		doCheck();
	}
	
	private void doCheck() {
		if (recordSelection != null && splitCopybook != CopybookLoader.SPLIT_NONE) {
			throw new RuntimeException("A record selection can only be specified when split=None");
		}
	}
	

	public ExternalRecord createExternalRecord() throws Exception {
		
		ICopybookLoaderStream loader = parent.getLoader();
		if (loader instanceof ISetDropCopybookName) { 
			((ISetDropCopybookName) loader).setDropCopybookFromFieldNames(parent.isDropCopybookNameFromFields());
		}

		ExternalRecord r = createExternalRecordImp();
		
		if (recordSelection != null) {
			r.setRecordSelection(recordSelection);
		}
		
		return r;
	}
	
	public abstract ExternalRecord createExternalRecordImp() throws Exception;

}
