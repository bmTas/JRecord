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

    public static final int SPLIT_NONE     = 0;
    public static final int SPLIT_REDEFINE = 1;
    public static final int SPLIT_01_LEVEL = 2;

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
    //        SAXException, ParserConfigurationException, RecordException;
}