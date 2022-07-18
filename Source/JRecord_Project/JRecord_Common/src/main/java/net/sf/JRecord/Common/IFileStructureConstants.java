package net.sf.JRecord.Common;

/**
 * List the IO modes available in JRecord
 * 
 * @author Bruce Martin
 *
 */
public interface IFileStructureConstants {
	public static final int IO_DEFAULT      = 0;
	public static final int IO_STANDARD_TEXT_FILE    = 1;
		/** alias for IO_STANDARD_TEXT_FILE*/
    public static final int IO_TEXT_LINE    = IO_STANDARD_TEXT_FILE;
    public static final int IO_FIXED_LENGTH_RECORDS = 2;
    	/** Alias for IO_FIXED_LENGTH_RECORDS */
    public static final int IO_FIXED_LENGTH = IO_FIXED_LENGTH_RECORDS;
    public static final int IO_BINARY_IBM_4680 = 3;
    public static final int IO_VB           = 4;
    public static final int IO_VB_DUMP      = 5;
    public static final int IO_VB_FUJITSU   = 7;
    public static final int IO_VB_GNU_COBOL = 8;
    public static final int IO_VB_OPEN_COBOL= IO_VB_GNU_COBOL;
    public static final int IO_BIN_TEXT     = 9;
    public static final int IO_FIXED_LENGTH_CHAR = 10;
    
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
     * read VBS files, there is now Write option. 
     */
    public static final int IO_VBS  = 12;
    public static final int IO_VB_DUMP2=14;

    public static final int IO_UNKOWN_FORMAT = 21 /* RecordEditor Format */;
    public static final int IO_WIZARD        = 22 /* RecordEditor Format */;

    public static final int IO_MICROFOCUS = 31;
    
    public static final int IO_FIXED_BYTE_ENTER_FONT = 35;
    public static final int IO_FIXED_CHAR_ENTER_FONT = 36;
    public static final int IO_TEXT_BYTE_ENTER_FONT  = 37;
    public static final int IO_TEXT_CHAR_ENTER_FONT  = 38;

    public static final int IO_CSV              = 44;
    public static final int IO_BIN_CSV          = 45;
    public static final int IO_UNICODE_CSV      = 46;

    public static final int IO_CSV_NAME_1ST_LINE              = 47;
    public static final int IO_BIN_CSV_NAME_1ST_LINE          = 48;
    public static final int IO_UNICODE_CSV_NAME_1ST_LINE      = 49;

    public static final int IO_NAME_1ST_LINE    = 51;
    public static final int IO_GENERIC_CSV      = 52 /* RecordEditor Format */;
//    public static final int IO_GENERIC_CSV_UNICODE = 53 /* RecordEditor Format */;
    /**
     * IO_BIN_NAME_1ST_LINE is for internal use
     */
    public static final int IO_BIN_NAME_1ST_LINE     = 54;
    public static final int IO_UNICODE_NAME_1ST_LINE = 55;



    public static final int IO_XML_USE_LAYOUT   = 61;
    public static final int IO_XML_BUILD_LAYOUT = 62;

//    public static final int IO_EVALUATE_DEFAULT_BINARY    = 80;
//    public static final int IO_EVALUATE_DEFAULT_FIXED     = 81;
//    public static final int IO_EVALUATE_DEFAULT_BIN_TEXT  = 82;
//    public static final int IO_EVALUATE_DEFAULT_TEXT      = 83;

    public static final int IO_STANDARD_UNICODE_TEXT_FILE  = 90;
    	/** Alias for IO_STANDARD_UNICODE_TEXT_FILE */
    public static final int IO_UNICODE_TEXT = IO_STANDARD_UNICODE_TEXT_FILE;

    public static final int IO_PROTO_DELIMITED      = 71;
    public static final int IO_PROTO_SINGLE_MESSAGE = 72;
    public static final int IO_PROTO_SD_DELIMITED   = 73;
    public static final int IO_PROTO_SD_SINGLE_MESSAGE = 74;
    public static final int IO_PROTO_TEXT = 75;
    public static final int IO_PROTO_JSON = 76;

    public static final int IO_THRIFT_FILE  = 81;
    public static final int IO_AVRO_FILE    = 91;

    public static final int IO_GETTEXT_PO   = 101;
    public static final int IO_TIP          = 102;

}
