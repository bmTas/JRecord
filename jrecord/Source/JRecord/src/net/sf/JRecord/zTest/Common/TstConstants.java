/*
 * @Author Bruce Martin
 * Created on 11/11/2005
 *
 * Purpose:
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

package net.sf.JRecord.zTest.Common;

import java.io.File;

/**
 * Constants for testing
 *
 * @author Bruce Martin
 *
 */
public final class TstConstants {

	public static final String[] EBCDIC_SINGLE_BYTE_CHARSETS =  {
		"IBM037",		"IBM1047",		"IBM273",		"IBM280",
		"IBM285",		"IBM297",		"IBM500",		"IBM930",
		"IBM935",		"IBM937",
		
		"cp037",	"cp273",	"cp277",	"cp278",
		"cp280",	"cp284",	"cp285",	"cp290",
		"cp297",	"cp420",	"cp424",
		"cp500",	"cp833",		"cp838",
		"cp870",	"cp871",	"cp875",	
		"cp1025",	"cp1026",	"cp1112",
		"cp1122",	"cp1123",	
		"cp1140",	"cp1141",	"cp1142",	"cp1143",
		"cp1144",	"cp1145",	"cp1146",	"cp1147",
		"cp1148",	"cp1149",	

		"CP1047",		"CP930",	"CP935",		"CP937",	
		
//		"cp423",	"cp836",	"cp880",	"cp1027",	"cp1130",	"cp1132",
//		"cp5123",	"cp8612",	"cp12708",	"cp28709",	"cp62211",	"cp62224",
//		"cp62235",	"cp62245",
	};

    public static final String BM_DIRECTORY     = "/home/knoppix/";
    public static final String RE_DIRECTORY  ;//   = "G:\\Users\\Bruce01\\.JRecord\\Params\\"; 
    public static final String TEMP_DIRECTORY;//   = "G:\\Temp\\RecordEditorTest\\";   F:/
    static {
    	String thisClass = "TstConstants.class";
    	String file = TstConstants.class.getResource( thisClass).getFile();
    	RE_DIRECTORY = file.substring(0, file.length() - thisClass.length());
		
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
    public static final String SAMPLE_DIRECTORY = RE_DIRECTORY + "SampleFiles/";
    public static final String COBOL_DIRECTORY  = RE_DIRECTORY + "CopyBook/Cobol/";
    public static final String COBOL_DIRECTORY2 = RE_DIRECTORY + "CobolTestData/"; //"G:\\RecordEditor\\CobolTestData\\";
    public static final String RE_XML_DIRECTORY = RE_DIRECTORY + "CopyBook/Xml/";

    public static final String CSV_DIRECTORY        = RE_DIRECTORY + "CopyBook/csv/";
    public static final String CSV_DIRECTORY_OUTPUT = RE_DIRECTORY + "CopyBook/csv/";
    public static final String XML_DIRECTORY        = RE_DIRECTORY + "CopyBook/Xml/";
    public static final String XML_DIRECTORY_OUTPUT = RE_DIRECTORY + "CopyBook/Xml/";

//    public static final String COBOL_TEST_DIR = BM_DIRECTORY + "open-cobol-1.0/CobolSrc/";

    public static final String COBOL_TEST_DIR = COBOL_DIRECTORY2; //"G:\\RecordEditor\\CobolTestData\\";
//    /**
//     * tst constants
//     */
//    private TstConstants() {
//        super();
//    }
//    
//    public static void main(String[] a) {
//    	
//    }
}
