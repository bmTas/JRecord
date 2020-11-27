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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import net.sf.JRecord.External.ExternalRecord;

public class CreateExternalFromStream  extends CreateExternalBase  implements ICreateExternal  {

	private final String copybookName;
	private InputStream inStream;
	private byte[] copybookBytes = null ;

	

	public CreateExternalFromStream(IGetLoader parent, InputStream inStream, String copybookName) {
		super(parent, copybookName);
		this.copybookName = copybookName;
		this.inStream = inStream;
	} 

	@Override
	protected ExternalRecord createExternalRecordImp() throws Exception {
		
		if (copybookBytes == null) {
			ByteArrayOutputStream os = new ByteArrayOutputStream(0x8000);
			byte[] buf = new byte[0x8000];
			int l = inStream.read(buf);
			while (l > 0) {
				os.write(buf, 0, l);
				l = inStream.read(buf);
			}
			copybookBytes = os.toByteArray();
			inStream = null;
		}

		return parent. getLoader()
				.loadCopyBook(new ByteArrayInputStream(copybookBytes), copybookName, splitCopybook, 0, 
						parent.getFont(), parent.getCopybookFileFormat(), 
						parent.getDialect(), 0, parent.getLog());
	}
}
