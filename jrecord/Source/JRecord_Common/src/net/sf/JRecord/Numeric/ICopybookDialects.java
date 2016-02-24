package net.sf.JRecord.Numeric;

public interface ICopybookDialects {

	public static final int FMT_INTEL = 0;
	public static final int FMT_MAINFRAME = 1;
	public static final int FMT_FUJITSU = 2;
	public static final int FMT_BIG_ENDIAN = 3;
	public static final int FMT_OPEN_COBOL = 4;
	public static final int FMT_FS2000 = 5;
	public static final int FMT_OPEN_COBOL_MVS = 6;
	public static final int FMT_OC_MICRO_FOCUS = 7;
	public static final int FMT_OPEN_COBOL_BE = 8;
	public static final int FMT_FS2000_BE = 9;
	public static final int FMT_OPEN_COBOL_MVS_BE = 10;
	public static final int FMT_OC_MICRO_FOCUS_BE = 11;
	public static final int FMT_MICRO_FOCUS = 21;
	public static final int FMT_MAINFRAME_COMMA_DECIMAL = 31;
	public static final int FMT_FUJITSU_COMMA_DECIMAL = 32;
	
	public static final int FMT_GNU_COBOL = FMT_OPEN_COBOL;
	public static final int FMT_GNU_COBOL_MVS = FMT_OPEN_COBOL_MVS;
	public static final int FMT_GNU_COBOL_MF = FMT_OC_MICRO_FOCUS;


}