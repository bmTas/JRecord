/*
 * @Author Bruce Martin
 * Created on 22/01/2007
 *
 * Purpose:
 *   Interface of a Dabase loader class
 */
package net.sf.JRecord.External;

import net.sf.JRecord.Log.AbsSSLogger;



/**
 * description of a class to load a copybook (i.e. Record Layout or Record Description)
 * into a ExteranlRecord (general purpose interface class).
 *
 * @author Bruce Martin
 *
 */
public interface CopybookLoader {

//    public static final int FMT_INTEL      = 0;
//    public static final int FMT_MAINFRAME  = 1;
//    public static final int FMT_FUJITSU    = 2;
//    public static final int FMT_BIG_ENDIAN = 3;

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

    /**
     * Read an Copybook from a file into the internal exchange format (ExternalRecord)
     * This can be converted to a LayoutDetail via
     *
     * @param copyBookFile Copy Book file Name
     * @param splitCopybookOption weather to split a copy book on a redefine
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return Copybook that has been read in  (ExternalRecord)
     *
     * @throws Exception any error that occurs
     */
    public abstract ExternalRecord loadCopyBook(final String copyBookFile,
            final int splitCopybookOption, final int dbIdx, final String font,
            final int binFormat,
            final int systemId,
            final AbsSSLogger log) throws Exception;
    
    public abstract ExternalRecord loadCopyBook(final String copyBookFile,
            final int splitCopybookOption, final int dbIdx, final String font,
            final int copybookFormat,
            final int binFormat,
            final int systemId,
            final AbsSSLogger log) throws Exception;

    //        SAXException, ParserConfigurationException, RecordException;
}