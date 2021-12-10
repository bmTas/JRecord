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

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.cb2xml.copybookReader.IReadCobolCopybook;

public class CreateExternalFromCopybookReader extends CreateExternalBase implements ICreateExternal  {

	private final IReadCobolCopybook copybookReader;
//	private final CblIOBuilderMultiSchema parent;

	

	public CreateExternalFromCopybookReader(IGetLoader parent, IReadCobolCopybook copybookReader) {
		super(parent, copybookReader.getCopybookName()); 
		this.copybookReader = copybookReader;
	} 


	@Override
	protected ExternalRecord createExternalRecordImp() throws Exception {
		ICopybookLoaderStream loader = parent.getLoader();
		
		if (! (loader instanceof CobolCopybookLoader)) {
			throw new RecordException("A cobol Copybook reader can only be used with Cobol-Copybooks");
		}
		
		return ((CobolCopybookLoader) loader).loadCopyBook(copybookReader, splitCopybook, 0, parent.getFont(), 
				parent.getCopybookFileFormat(), parent.getDialect(), 0, parent.getLog());
		
	}
}
