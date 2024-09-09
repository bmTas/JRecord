package net.sf.JRecord.Common;

/**
 * List the IO modes available in JRecord
 * 
 * @author Bruce Martin
 *
 */
public interface IFileStructureConstants {
	int IO_DEFAULT      = 0;
	/**
	 * Standard Windows/Linux/Unix/Mac ascii text file with \n line ending's.
	 * It is read as bytes (Java Stream). 
	 * For Unicode use IO_UNICODE_TEXT
	 */
	int IO_STANDARD_TEXT_FILE    = 1;
		/** alias for IO_STANDARD_TEXT_FILE*/
    int IO_TEXT_LINE    = IO_STANDARD_TEXT_FILE;
    /**
     * Each line is a Fixed length. There are no line ending characters.
     * The file is read as bytes so supports binary fields\ but not unicode.
     * For unicode files use IO_FIXED_LENGTH_CHAR
     */
    int IO_FIXED_LENGTH_RECORDS = 2;
    	/** Alias for IO_FIXED_LENGTH_RECORDS */
    int IO_FIXED_LENGTH = IO_FIXED_LENGTH_RECORDS;
    int IO_BINARY_IBM_4680 = 3;
    /**
     * Mainframe recfm=VB file. Each line consists of a line length followed by the lines data. 
     * There is no Block information, just the file data.
      */
    int IO_VB           = 4;
    /**
     * Mainframe Recfm=VB including, It includes both disk-block data + the file data.
     * IO_VB contains file data but no block data.
     */
    int IO_VB_DUMP      = 5;
    /**
     * Fujitsu Cobols VB files. They contain line-lengths at both the start and end of the line.
     */
    int IO_VB_FUJITSU   = 7;
    /**
     * GNU Cobols VB files. Each line consists of a line-length followed by the Line Data.
     */
    int IO_VB_GNU_COBOL = 8;
    int IO_VB_OPEN_COBOL= IO_VB_GNU_COBOL;
    /**
     * Text file, it is read as byte (java stream) instead of as Characters. It does not handle Unicode.
     */
    int IO_BIN_TEXT     = 9;
    /**
     * Each line (or record) is a fixed number of characters. There are no line ending characters.
     * It will support Unicode files but not binary files.
     */
    int IO_FIXED_LENGTH_CHAR = 10;
    
    /**
     * IO_CONTINOUS_NO_LINE_MARKER is for files where there are no 
     * <i>End-of-Line</i> markers. Records are based on Record-Lengths.
     * <p>If there are more than 1 record type, you <b>MUST</b> set 
     * record-Selection criteria <b>!!!</b> 
     * 
     * <p>Also <b>try and</b> <font color="red"><b>Avoid</b></font> using 
     * <b>IO_CONTINOUS_NO_LINE_MARKER</b> if you can, it is always going to be error prone. 
     * I have provided this option just in case there is no other choice.</p>
     */
    public static final int IO_CONTINOUS_NO_LINE_MARKER  = 11;
    
    /**
     * @deprecated This option is for reading Mainframe VBS files,
     * It was written based on information in the Manual and has had
     * very limited testing against real VBS files.
     * <p>I <b>strongly</b> suggest you <b>copy</b> the file from <b>VBS</b> to <b>VB</b>/<b>FB</b> on the mainframe
     * and use <b>IO_VB</b> / <b>IO_FIXED_LENGTH</b> instead of the IO_VBS option. Also you can only 
     * read VBS files, there is no Write option. 
     */
    int IO_VBS  = 12;
    int IO_VB_DUMP2=14;

    int IO_UNKOWN_FORMAT = 21 /* RecordEditor Format */;
    int IO_WIZARD        = 22 /* RecordEditor Format */;

    int IO_MICROFOCUS = 31;
    
    int IO_FIXED_BYTE_ENTER_FONT = 35;
    int IO_FIXED_CHAR_ENTER_FONT = 36;
    int IO_TEXT_BYTE_ENTER_FONT  = 37;
    int IO_TEXT_CHAR_ENTER_FONT  = 38;

    int IO_CSV              = 44;
    int IO_BIN_CSV          = 45;
    int IO_UNICODE_CSV      = 46;

    int IO_CSV_NAME_1ST_LINE              = 47;
    int IO_BIN_CSV_NAME_1ST_LINE          = 48;
    int IO_UNICODE_CSV_NAME_1ST_LINE      = 49;

    int IO_NAME_1ST_LINE    = 51;
    int IO_GENERIC_CSV      = 52 /* RecordEditor Format */;
//    int IO_GENERIC_CSV_UNICODE = 53 /* RecordEditor Format */;
    /**
     * IO_BIN_NAME_1ST_LINE is for internal use
     */
    int IO_BIN_NAME_1ST_LINE     = 54;
    int IO_UNICODE_NAME_1ST_LINE = 55;



    int IO_XML_USE_LAYOUT   = 61;
    int IO_XML_BUILD_LAYOUT = 62;

//    public static final int IO_EVALUATE_DEFAULT_BINARY    = 80;
//    public static final int IO_EVALUATE_DEFAULT_FIXED     = 81;
//    public static final int IO_EVALUATE_DEFAULT_BIN_TEXT  = 82;
//    public static final int IO_EVALUATE_DEFAULT_TEXT      = 83;

    int IO_STANDARD_UNICODE_TEXT_FILE  = 90;
    	/** Alias for IO_STANDARD_UNICODE_TEXT_FILE */
    int IO_UNICODE_TEXT = IO_STANDARD_UNICODE_TEXT_FILE;

    int IO_PROTO_DELIMITED      = 71;
    int IO_PROTO_SINGLE_MESSAGE = 72;
    int IO_PROTO_SD_DELIMITED   = 73;
    int IO_PROTO_SD_SINGLE_MESSAGE = 74;
    int IO_PROTO_TEXT = 75;
    int IO_PROTO_JSON = 76;

    int IO_THRIFT_FILE  = 81;
    int IO_AVRO_FILE    = 91;

    int IO_GETTEXT_PO   = 101;
    int IO_TIP          = 102;

}
