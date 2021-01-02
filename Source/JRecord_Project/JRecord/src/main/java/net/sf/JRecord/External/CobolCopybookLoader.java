/**
 * 
 */
package net.sf.JRecord.External;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.base.BaseCobolItemLoader;
import net.sf.JRecord.External.cb2xml.CobolCopybookReader;
import net.sf.JRecord.Log.AbsSSLogger;


/**
 * @author Bruce Martin
 */
public class CobolCopybookLoader extends BaseCobolItemLoader<ExternalRecord> 
implements ICopybookLoaderStream, ICopybookLoaderCobol  {

	private static boolean available = true;
    private static boolean toCheck = true;

	/**
	 * @param useJRecordNaming
	 * @param recBuilder
	 */
	public CobolCopybookLoader() {
		super(true, new ExternalRecordBuilder(), new CobolCopybookReader());
	}
	
	/**
     * Insert a XML Dom Copybook into the Copybook DB
     *
     * @param copyBookName Copy Book file Name
     * @param splitCopybook wether to split a copy book on a redefine / 01
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binaryFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return return the record that has been read in
     */
    public final ExternalRecord loadCopyBook(
    								InputStream inputStream, //Document copyBookXml,
    		                             String copyBookName,
            						  		int splitCopybook,
            						  		int dbIdx,
                  						  final String font,
                						  final int binaryFormat,
                						  final int systemId,
                						  final AbsSSLogger log)
    				{
    	try {
			return loadCopyBook(inputStream, copyBookName, splitCopybook, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binaryFormat, systemId, log);
		} catch (IOException e) {
			throw new RecordException("IO Exception: " + e, e);

		}
    }

   /**
     * wether cb2xml is available (needed for converting a Cobol Copybook
     * to a XML Dom representation
     *
     * @return wether cb2xml is available on the class path
     */
    public static final boolean isAvailable() {

        if (toCheck) {
            try {
                /*
                 * try to load CobolPreprocessor to see if the cb2xml jar is present
                 * I use the CobolPreprocessor because it only uses IO classes.
                 * This aviods loading unnessary classes before need be
                 */
                available = ((new CobolCopybookLoader()).getClass().getClassLoader().getResource("net/sf/cb2xml/Cb2Xml.class") != null);
            } catch (Exception e) {
                available = false;
            }
            toCheck = false;
        }
        return available;
    }

}
