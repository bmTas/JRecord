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
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public abstract class CreateExternalBase {
	final IGetLoader parent;
	private final String recordName;
	private ExternalRecord lastExternalRecord;
	
	private boolean optimizeTypes = true;
	
	int splitCopybook = CopybookLoader.SPLIT_NONE;
	private ExternalSelection recordSelection = null;
	private IStartingPosition startPosition = new IStartingPosition() {		
		@Override public int calculateStartingPosition() {
			return 0;
		}
	};
	

	protected CreateExternalBase(IGetLoader parent, String name) {
		super();
		this.parent = parent;
		this.recordName = name;
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
	
	
	
	public final void setStartPosition(IStartingPosition startPosition) {
		this.startPosition = startPosition;
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

		lastExternalRecord = createExternalRecordImp();
		lastExternalRecord.setOptimizeTypes(optimizeTypes);
		
		if (recordSelection != null) {
			lastExternalRecord.setRecordSelection(recordSelection);			
		}
		int startAdj = startPosition.calculateStartingPosition();
		if (startAdj > 0) {
			adjustFields(lastExternalRecord, startAdj);
		}
		
		return lastExternalRecord;
	}
	
	
	
	public ExternalRecord getLastExternalRecord() {
		return lastExternalRecord;
	}

	public void clearLastRecord() {
		lastExternalRecord = null;
	}

	public String getRecordName() {
		return recordName;
	}


	private void adjustFields(ExternalRecord r, int adj) {
		for (int i = 0; i < r.getNumberOfRecords(); i++) {
			adjustFields(r.getRecord(i), adj);
		}
		for (int i = 0; i < r.getNumberOfRecordFields(); i++) {
			ExternalField f = r.getRecordField(i);
			f.setPos(f.getPos() + adj);
		}
	}
	
	protected abstract ExternalRecord createExternalRecordImp() throws Exception;


	/**
	 * @param optimizeTypes the optimizeTypes to set
	 */
	public void setOptimizeTypes(boolean optimizeTypes) {
		this.optimizeTypes = optimizeTypes;
	}
}
