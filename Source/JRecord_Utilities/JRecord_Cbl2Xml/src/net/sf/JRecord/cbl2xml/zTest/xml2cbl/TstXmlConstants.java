/*
 * @Author Bruce Martin
 * Created on 11/11/2005
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import java.io.File;

/**
 * Constants for testing
 *
 * @author Bruce Martin
 *
 */
public final class TstXmlConstants {


    public static final String TX_DIRECTORY  ;//   = "G:\\Users\\Bruce01\\.JRecord\\Params\\"; 
    public static final String TEMP_DIRECTORY;//   = "G:\\Temp\\RecordEditorTest\\";   F:/
    static {
    	String thisClass = "TstXmlConstants.class";
    	String file = TstXmlConstants.class.getResource( thisClass).getFile();
    	TX_DIRECTORY = file.substring(0, file.length() - thisClass.length());
		
		//
		String defaultTempDirectory = "G:\\Temp\\JRecordTest\\";
		if (new File(defaultTempDirectory) .exists() ) {
			TEMP_DIRECTORY = defaultTempDirectory;
		} else {
			TEMP_DIRECTORY = System.getProperty("java.io.tmpdir") + File.separator;
		}
		
		//System.out.println(RE_DIRECTORY + " " + TEMP_DIRECTORY + " " + System.getProperty("java.io.tmpdir"));
    }
    public static final int    DB_INDEX         = 0;
    public static final String XML_DIRECTORY = TX_DIRECTORY + "xml/";
    public static final String COBOL_DIRECTORY = TX_DIRECTORY + "cobol/";
}
