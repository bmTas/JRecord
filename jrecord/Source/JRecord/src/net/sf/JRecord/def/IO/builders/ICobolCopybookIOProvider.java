package net.sf.JRecord.def.IO.builders;

import java.io.InputStream;

/**
 * Interface to create CobolIOBuilders (a Builder builder).
 * <pre>{@code
 *      AbstractLineReader r = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 * }</pre> 

 * @author Bruce Martin
 *
 */
public interface ICobolCopybookIOProvider {

	/**
	 * Create a new Cobol IOBulder from a COBOL-Copybook file
	 * 
	 *<pre>
	 *<b>Example:</b>
	 * 
	 *      AbstractLineReader r = JRecordInterface1.COBOL
	 *              .<b>newIOBuilder("file-name")</b>
	 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
	 *              .newReader("Data-Filename");
	 * </pre> 
	 * 
	 * @param copybookFileame name of the COBOL-Copybook stream.
	 * 
	 * These are the default values (which can be overriden with the appropriate set* method
	 * @return requested IOBuilder
	 * @author Bruce Martin
	 * 
	 *<pre> </pre>
	 *
	 * <b>Main Methods:</b><ul>
	 *  <li><b>setFileOrganization</b> Set the <i>file organization</i> (or Structure). While a Windows/Unix style
	 * file organization is the most common (supported by Constants.<b>IO_STANDARD_TEXT_FILE</b> or Constants.<b>IO_UNICODE_TEXT</b> in JRecord).
	 * There others including (but is not limited to):
	 * <pre>
	 *     <b>Variable Length</b> where the length is before the Record Data:
	 *     
	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
	 *           
	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length:
	 *     
	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
	 *          
	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
	 * </pre>
	 *  <li><b>setSplitCopybook</b> Wether the Cobol-Copybook should be split into sub-record's or not and how it should be split.
	 *  <li><b>setDialect</b Set the Cobol dialect (is it Mainframe, GNU-Cobol etc).
	 *  <li><b>setCopybookFileFormat</b> - is a standard Column 6-72 or some other format
	 *  <li><b>setFont</b> Set the font (character-set) used in the Data-File.
	 *  </ul>
	 * 
	 */
	public abstract ICobolIOBuilder newIOBuilder(
			String copybookFileame);

	/**
	 * Create a new Cobol IOBulder for a file.
	 * 
	 *<pre>
	 *<b>Example:</b>
	 *       
	 *      AbstractLineReader r = JRecordInterface1.COBOL
	 *             .<b>newIOBuilder(cobolCopybookStream, "My-Cobol-Record")</b>
	 *                 .setFileOrganization(Constants.IO_FIXED_LENGTH)
	 *                 .setDialect(ICopybookDialects.FMT_FUJITSU)
	 *             .newReader("Data-Filename");
	 * </pre> 

	 * @param copybookFileame name of the Copybook (or schema file).
	 * @param cobolDialect Cobol Dialect. Values include:<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe cobol
	 *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - Open cobol (or GNU Cobol as it is now known).
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Old Free Fujitsu Cobol 3. 
	 * </ul>
	 * 
	 * These are the default values (which can be overriden with the appropriate set* method)
	 * @return requested IOBuilder
	 * 
	 *<pre> </pre>
	 *
	 * <b>Main Methods:</b><ul>
	 *  <li><b>setFileOrganization</b> Set the <i>file organization</i> (or Structure). While a Windows/Unix style
	 * file organization is the most common (supported by Constants.<b>IO_STANDARD_TEXT_FILE</b> or Constants.<b>IO_UNICODE_TEXT</b> in JRecord).
	 * There others including (but is not limited to):
	 * <pre>
	 *     <b>Variable Length</b> where the length is before the Record Data:
	 *     
	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
	 *           
	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length:
	 *     
	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
	 *          
	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
	 * </pre>
	 *  <li><b>setSplitCopybook</b> Wether the Cobol-Copybook should be split into sub-record's or not and how it should be split.
	 *  <li><b>setDialect</b Set the Cobol dialect (is it Mainframe, GNU-Cobol etc).
	 *  <li><b>setCopybookFileFormat</b> - is a standard Column 6-72 or some other format
	 *  <li><b>setFont</b> Set the font (character-set) used in the Data-File.
	 *  </ul>
	 * 
	 */
	public abstract ICobolIOBuilder newIOBuilder(
			InputStream cobolCopybookStream, String copybookName);

}