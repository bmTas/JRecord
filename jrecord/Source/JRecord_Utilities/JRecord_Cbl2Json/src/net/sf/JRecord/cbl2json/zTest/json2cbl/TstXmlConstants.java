/*
 * @Author Bruce Martin
 * Created on 11/11/2005
 *
 * Purpose:
 */
package net.sf.JRecord.cbl2json.zTest.json2cbl;

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
