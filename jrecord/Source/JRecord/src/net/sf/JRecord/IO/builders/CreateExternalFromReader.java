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

import java.io.Reader;
import java.io.StringReader;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;

public class CreateExternalFromReader  extends CreateExternalBase  implements ICreateExternal  {

	private final String copybookName;
	private Reader reader;
	private String copybookStr = null ;

	

	public CreateExternalFromReader(IGetLoader parent, Reader reader, String copybookName) {
		super(parent, copybookName);
		this.copybookName = copybookName;
		this.reader = reader;
	} 

	@Override
	public ExternalRecord createExternalRecordImp() throws Exception {
		
		if (copybookStr == null) {
			char[] buf = new char[0x8000];
			int l = reader.read(buf);
			StringBuilder copybookSB = new StringBuilder();
			while (l > 0) {
				copybookSB.append(buf, 0, l);
				l = reader.read(buf);
			}
			reader = null;
			copybookStr = copybookSB.toString();
		}

		
		ICopybookLoaderStream loader = parent. getLoader();
		
		if (loader instanceof ICopybookLoaderStream) {
			return ((ICopybookLoaderStream)loader)
				.loadCopyBook(new StringReader(copybookStr), copybookName, splitCopybook, 0, 
						parent.getFont(), parent.getCopybookFileFormat(), 
						parent.getDialect(), 0, parent.getLog());
		}
		throw new RecordException("Internal Error: loader is not a Cobol Copybook loader");
	}
}
