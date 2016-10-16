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

import net.sf.JRecord.Common.BasicNamedManager;
import net.sf.JRecord.External.base.CopybookWriter;
import net.sf.JRecord.External.base.RecordEditorCSVWriter;
import net.sf.JRecord.External.base.RecordEditorXmlWriter;

/**
 * Class to manage all the Copybook Writer class's
 *
 * @author Bruce Martin
 *
 */
public class CopybookWriterManager extends BasicNamedManager<CopybookWriter> {

	public static final int RECORD_EDITOR_TAB_CSV_WRITER = 0;
	public static final int RECORD_EDITOR_XML_WRITER = 1;

	private static final int NUMBER_OF_WRITERS = 5; 
	private static CopybookWriterManager instance = null;



	public CopybookWriterManager() {
		super("Copybook_Writers", NUMBER_OF_WRITERS, NUMBER_OF_WRITERS, new CopybookWriter[NUMBER_OF_WRITERS]);
		register(0, "RecordEditor (Tab) CSV", new RecordEditorCSVWriter("\t"));
		register(1, "RecordEditor XML", new RecordEditorXmlWriter());
//		ExternalConversion.setStandardConversion(this);
	}


	/**
	 * @return the instance
	 */
	public static CopybookWriterManager getInstance() {
		if (instance == null) {
			instance = new CopybookWriterManager();
		}
		return instance;
	}
}
