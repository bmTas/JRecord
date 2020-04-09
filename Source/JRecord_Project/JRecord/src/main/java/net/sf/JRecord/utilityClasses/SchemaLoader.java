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

package net.sf.JRecord.utilityClasses;

import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;

/**
 * This class is used by some of the utilities. 
 * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
 * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes
 * 
 * @author Bruce Martin
 *
 */
public class SchemaLoader {

	/**
	 * This method is used by several utilities. 
	 * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes
     * 
	 * @param schemaFileName name of the schema/Cobol file
	 * @param split Cobol split option
	 * @param fontname character-set
	 * @param binformat Cobol Dialect.
	 * 
	 * @return External Record
	 * @throws Exception
	 */
	public static ExternalRecord loadSchema(String schemaFileName, int split, String fontname, int binformat) 
	throws Exception  {
		
		CopybookLoader conv;
		if (schemaFileName.toLowerCase().endsWith(".xml")) {
			conv = new RecordEditorXmlLoader();
		} else {
			conv = new CobolCopybookLoader();	
		}
		
		return conv.loadCopyBook(schemaFileName, split, 0, fontname, binformat, 0, new TextLog()); 
	}
}
