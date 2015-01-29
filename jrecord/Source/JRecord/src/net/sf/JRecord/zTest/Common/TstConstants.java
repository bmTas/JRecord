/*
 * @Author Bruce Martin
 * Created on 11/11/2005
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.Common;

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
    public static final String RE_DIRECTORY     = "G:\\Users\\Bruce01\\.JRecord\\Params\\";
    public static final String TEMP_DIRECTORY   = "G:\\Temp\\RecordEditorTest\\";
    public static final int    DB_INDEX         = 0;
    public static final String SAMPLE_DIRECTORY = RE_DIRECTORY + "SampleFiles/";
    public static final String COBOL_DIRECTORY  = RE_DIRECTORY + "CopyBook/Cobol/";
    public static final String COBOL_DIRECTORY2 = "G:\\RecordEditor\\CobolTestData\\";
    public static final String RE_XML_DIRECTORY = RE_DIRECTORY + "CopyBook/Xml/";

    public static final String CSV_DIRECTORY        = RE_DIRECTORY + "CopyBook/csv/";
    public static final String CSV_DIRECTORY_OUTPUT = RE_DIRECTORY + "CopyBook/csv/";
    public static final String XML_DIRECTORY        = RE_DIRECTORY + "CopyBook/Xml/";
    public static final String XML_DIRECTORY_OUTPUT = RE_DIRECTORY + "CopyBook/Xml/";

//    public static final String COBOL_TEST_DIR = BM_DIRECTORY + "open-cobol-1.0/CobolSrc/";

    public static final String COBOL_TEST_DIR = "G:\\RecordEditor\\CobolTestData\\";
    /**
     * tst constants
     */
    private TstConstants() {
        super();
    }
}
