package net.sf.JRecord.Option;


/**
 * Options to split a Cobol Copybook into seperate records
 * 
 * @author Bruce Martin
 *
 */
public interface ICobolSplitOptions {
	
	/** Standard Single record copybook */
    public static final int SPLIT_NONE     = 0;
    /** 
     *  Multi-Record Copybook with each record is in a redefines
     *  <pre>
     *     01  Trans-File.
     *         .....
     *        03 Record-Type                  Pic X.
     *        ....
     *        03  Header-Record.
     *            ..... 
     *        03  Detail-Record redefines Header-Record.
     *  </pre>
     */
    public static final int SPLIT_REDEFINE = 1;
    /** 
     *  Multi-Record Copybook with each record on a 01 Group level
     *  <pre>
     *     01  Header-Record.
     *         .....
     *     01  Detail-Record.
     *  </pre>
     */
    public static final int SPLIT_01_LEVEL = 2;
    /** 
     *  Multi-Record Copybook with each record in a Group level under 01 
     *  i.e. 05 in the following 
     *  <pre>
     *        05  Header-Record.
     *            .....
     *        05  Detail-Record.
     *  </pre>
     */
    public static final int SPLIT_HIGHEST_REPEATING = 3;

}
