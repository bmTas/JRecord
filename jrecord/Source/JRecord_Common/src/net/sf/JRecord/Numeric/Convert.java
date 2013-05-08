/*
 * @Author Bruce Martin
 * Created on 23/03/2007
 *
 * Purpose:
 * This interface descibes a class that will convert
 * a Cobol Numeric Type into a JRecord.type identifier
 */
package net.sf.JRecord.Numeric;

/**
 * This interface describes a class that will convert
 * a Cobol Type (picture / usage) into a JRecord.type identifier
 *
 * @author Bruce Martin
 *
 */
public interface Convert {

    public static final int FMT_INTEL      = 0;
    public static final int FMT_MAINFRAME  = 1;
    public static final int FMT_FUJITSU    = 2;
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
    public static final int FMT_MAINFRAME_COMMA_DECIMAL  = 31;
    public static final int FMT_FUJITSU_COMMA_DECIMAL  = 32;

    /**
     * Get the Binary Definition details
     *
     * @return Get the Binary Definition details
     */
    public abstract Object getNumericDefinition();
//  public abstract net.sf.cb2xml.def.NumericDefinition getNumericDefinition();
//  using Object instead of net.sf.cb2xml.def.NumericDefinition to avoid dependency on cb2xml when
//  it is otherwise not needed

    /**
     * This method will convert a
     * @param usage Cobol usage clause i.e. Comp Comp-3 etc
     * @param picture picture Cobol picture - s9(6)v99.
     * @param signed - wether it is a signed field
     * @return Jrecord Type Code
     */
    public abstract int getTypeIdentifier(String usage, String picture, boolean signed, boolean signSeperate, String signPosition);

    /**
     * Get the conversion Identifier
     * @return conversion Identifier
     */
    public abstract int getIdentifier();

    /**
     * Get the binary id to use
     * @return actual binary Id
     */
    public abstract int getBinaryIdentifier();

    /**
     * Get the file structure
     *
     * @param multipleRecordLengths wether there are multiple records
     * @param binary wether it is a binary file
     *
     * @return File Structure to use.
     */
    public abstract int getFileStructure(boolean multipleRecordLengths, boolean binary);

    /**
     * Get the name
     * @return the Name
     */
    public String getName();
}
