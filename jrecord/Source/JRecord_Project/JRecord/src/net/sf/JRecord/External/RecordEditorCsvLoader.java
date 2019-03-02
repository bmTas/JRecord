/*
 * @Author Bruce Martin
 * Created on 7/02/2007
 *
 * Purpose:
 * Demonstrate writing a CopybookLoader (ie a class to read
 * a record layout or Copybook from an external file).
 */
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

import net.sf.JRecord.External.base.BaseRecordEditorCsvLoader;

/**
 * This class reads a Record Layout (Copybook) stored in a  tab delimited file.
 * Fields in the file are<ol>
 * <li>Starting Position
 * <li>Length
 * <li>Number of places after the decimal point
 * <li>Field Name
 * <li>Field Type (String [or char], num, mainframe_zoned, fuji_zoned)
 * <li>Decimal - number of places after the decimal point
 * <li>Format Field Format (used in the Record Editor)
 * <li>Parameter - parameter for the Type / Format
 * </ol>
 *
 * <pre>
 *   <b>Usage:</b>
 *        CopybookLoader loader = new RecordEditorCsvLoader(",");
 *        LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class RecordEditorCsvLoader {


	   /**
     * CSV Parser - Tab
     * @author Bruce Martin
     */
    public static final class Tab 
    extends BaseRecordEditorCsvLoader<ExternalRecord> 
    implements ICopybookLoaderStream {
    	public Tab() {
    		super(new ExternalRecordBuilder(), "\t");
    	}
    }


    /**
     * CSV Parser - Tab
     * @author Bruce Martin
     */
    public static final class Comma 
    extends BaseRecordEditorCsvLoader<ExternalRecord> 
    implements ICopybookLoaderStream {
    	public Comma() {
    		super(new ExternalRecordBuilder(), ",");
    	}
    }
}