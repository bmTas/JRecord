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

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.Log.AbsSSLogger;

public interface ICopybookLoaderStream extends CopybookLoader {

	/**
	 * Insert a XML Dom Copybook into the Copybook DB
	 *
	 * @param copyBookName Copy Book file Name
	 * @param splitCopybook wether to split a copy book on a redefine / 01
	 * @param dbIdx Database Index
	 * @param font font name to use
	 * @param copybookFormat format of the copybook; see cb2xmlConstants
	 * @param binaryFormat binary format to use
	 * @param systemId System Identifier
	 * @param log log where any messages should be written
	 *
	 * @return return the record that has been read in
	 */
	public abstract ExternalRecord loadCopyBook(
			InputStream inputStream, //Document copyBookXml,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException;
	
	
	public abstract ExternalRecord loadCopyBook(
			Reader reader, //Document copyBookXml,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException;

	
}