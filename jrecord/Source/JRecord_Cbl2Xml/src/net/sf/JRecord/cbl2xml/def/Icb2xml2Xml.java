package net.sf.JRecord.cbl2xml.def;

import java.io.IOException;
import java.io.InputStream; 
import java.io.OutputStream;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;

public interface Icb2xml2Xml  {
	public static final String MAIN_XML_TAG = "CobolData";

	/**
	 * File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
	 * The main options are:<ul>
	 *   <li><b>Constants.IO_DEFAULT</b> - JRecord will decide the actual method based on other values (The Default Value).
	 *   It is generally better to explicitly set the File-Organisation (or file-Structure).
	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard Windows/*nix/Mac text file using  \n, \n\r etc as a record (or line) delimiter.
	 *   <li><b>Constants.IO_UNICODE_TEXT</b> - Standard Windows/*nix/Mac Unicode / double byte text file using  \n, \n\r etc 
	 *   as a record (or line) delimiter. It ensures record are stored in character format (instead of bytes).
	 *   <li><b>Constants.IO_FIXED_LENGTH</b> - Every Record (or line) is a standard Fixed length based on the Maximum
	 *   schema (LayoutDetail) record length.
	 *   <li><b>Constants.IO_FIXED_LENGTH_CHAR</b> - Fixed length character file (typically used for Fixed-Length unicode files).
	 *   <li><b>Constants.IO_VB</b> - Mainframe VB (Variable Record length file). Records consist of a Record-Length followed by the Record-Data.
	 *   <li><b>Constants.IO_VB_DUMP</b> - Raw Block format of a Mainframe-VB file. You get this format  if you specify RECFM=U when reading it on the mainframe.
	 *   <li><b>Constants.IO_VB_OPEN_COBOL</b> - GNU (open-Cobol) VB format.
	 * </ul>
	 *<pre>
     *<b>Example:</b> 
	 *      {@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *}</pre> 
     *
     *
     *<pre>
	 *     <b>Variable Length</b> where the length is before the Record Data (IO_VB*):
	 *     
	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
	 *           
	 *          Record Record Data  	         
	 *          Length ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
     *              11 = Record 1>
     *              21 = Record 2 --------->
     *              61 = Record 3 ------------------------------------------------->
     *              25 = Record 4 ------------->
	 *           
	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length (IO_FIXED_LENGTH and IO_FIXED_LENGTH_CHAR:
	 *     
	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
	 *          
	 *          Fixed length file (Record Length = 15):
	 *          
	 *          ----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9
	 *          = Record 1 ---&gt;= Record 2 ---&gt;= Record 3 ---&gt;= Record 4 ---&gt;= Record 5 ---&gt;
	 *          
	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
	 * </pre>
	 * 
	 * @param fileOrganization File Organization (or File Structure)
	 */
	public abstract Icb2xml2Xml setFileOrganization(int fileOrganization);

	/**
	 * @param splitCopybook Wether to Split the Copybook or not. Options are:<ul>
	 *    <li><b>CopybookLoader.SPLIT_NONE</b> - No Split
	 *    <li><b>CopybookLoader.SPLIT_01_LEVEL</b> - Split on 01 levels
	 *    <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> - Split on the highest group level
	 *    in the copybook.
	 * </ul>
	 * <p>For Data2Xml, if you use SPLIT_01_LEVEL or SPLIT_HIGHEST_REPEATING, you should also
	 * <b>setRecordSelection</b> or <b>setRecordPositionCode</b>, see {@link setRecordSelection(String, int)}
	 * </pre>
	 *  
	 * <p>But the correct use is<ul>
	 * <li><b>CopybookLoader.SPLIT_NONE</b> where Records are redefined:
	 * <pre>
	 * 
	 *   01 Header-Record.
	 *      05 Record-Type                            Pic X.
     *         88 Header-Record         value 'H'.
     *      05 Creation-Date                           Pic 9(8).
     *      05 Version                                 pic 9(3)V99.
     *
     *  01 Detail-Record.
     *     05 Record-Type                            Pic X.
     *        88 Detail-Record         value 'D'.
     *     05 Field-1                                Pic X(10).
     *     05 Field-2                                Pic X(20).
     *     05 Field-3                                Pic X(10).
     *
     *   01 Trailer-Record.
     *      05 Record-Type                            Pic X.
     *         88 Trailer-Record        value 'T'.
     *      05 Record-Count                           Pic 9(9).
     *      
     * or
     * 
     *   01  Output-Record.
     *       05 Common-Record-Header.
     *          10 Record-type
     *             ...
     *       05 Record-1.
     *          .....
     *       05 Record-2 redefines Record-1.
     *          .....
     *       05 Record-3 redefines Record-1.
     *          .....
	 * </pre>
	 * <li><b>CopybookLoader.SPLIT_01_LEVEL</b> where Records are redefined:
	 * <pre>
	 * </pre>
	 *  01  Record-1.
     *      05 Record-Type
     *         ....
     *  01  Record-2.
     *      05 Record-Type
     *         ....
     *  01  Record-2.
     *      05 Record-Type
     *       ....
	 * </pre>
	 * <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> where Records are redefined:
	 * <pre>
	 * 
	 *         05  Header-Record.
	 *             ....
	 *         05  Detail-Record.
	 *             ....
	 *         05  Trailer-Record.
	 *             ....
	 * </pre>
	 * </ul> 
	 * 
	 * </pre>
	 */
	public abstract Icb2xml2Xml setSplitCopybook(int splitCopybook);


	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract Icb2xml2Xml setFont(String font);

	

	/**
	 * Define record-selection criteria. For a single selection you would do
	 * 
	 * @param recordName name of the record 
	 * @param selectionCriteria selection-criteria
	 * 
	 * @return updated IOBuilder
	 * 
	 * <pre>
	 * Usage:
	 * 
	 *   IOBldr.setRecordSelection("Header-Record", new FieldSelection("Record-Type", "H"));
	 * </pre>   
	 *  or if you want to use or's / and's  
	 * <pre>  
	 *   IOBldr.setRecordSelection(
     *          "Trailer-Record",
     *          ExternalGroupSelection.newOr(
     *                  new ExternalFieldSelection("Record-Type", "D"),
     *                  new ExternalFieldSelection("Record-Type", "E"),
     *                  new ExternalFieldSelection("Record-Type", "F")
     * 	 ));
     * 
     * This is basically the following expression:
     *    
     *         Record-Type = "D"
     *      or Record-Type = "E"
     *      or Record-Type = "F"
     *      
     * and more complicated boolean logic is possible:
     * 
     * 
     * 	 IOBldr.setRecordSelection(
     *          "Trailer-Record",
     *          ExternalGroupSelection.newAnd(
     *               new ExternalFieldSelection("Record-Type-1", "D"),
     *               ExternalGroupSelection.newOr(
     *                    new ExternalFieldSelection("Record-Type-2", "D"),
     *                    new ExternalFieldSelection("Record-Type-2", "E"),
     *                    new ExternalFieldSelection("Record-Type-2", "F")
     *               )
     *          ));
     *          
     *   which is
     *   
     *             Record-Type-1 = "D"
     *       and ( Record-Type-2 = "D"
     *          or Record-Type-2 = "E"
     *          or Record-Type-2 = "F" )     
	 * </pre>
	 * 
	 *
	 */
	public abstract Icb2xml2Xml setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	
	/**
	 * Set this as a position record (i.e. first or last record in the file)
	 * 
	 * @param recordName Name of Record to be updated 
	 * @param positionOption position type; options are <ul>
	 *     <li>Options.RP_FIRST_RECORD_IN_FILE
	 *     <li>Options.RP_MIDDLE_RECORDS
	 *     <li>Options.RP_LAST_RECORD_IN_FILE
	 * </ul>
	 * @return Builder for further updates
	 */
	public abstract ICobol2Xml setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

	
	/**
	 * Cobol is a column-sensitive language; Traditionally columns 1-5 are used for line-numbers (or version comment)
	 * and ignore everything after column 72. This parameter controls which part of the line to use. Supported values:<ul>
	 *   <li><b>Cb2xmlConstants.USE_STANDARD_COLUMNS</b> -  use columns 6-72 (normal format for mainframe copybooks), this is the default.
	 *   <li><b>Cb2xmlConstants.USE_COLS_6_TO_80</b> -  use columns 6-80
	 *   <li><b>Cb2xmlConstants.USE_LONG_LINE</b> -  use columns 6-10000
	 *   <li><b>Cb2xmlConstants.USE_PROPERTIES_FILE</b> -  columns are supplied in cb2xml.properties file.
	 * </ul>
	 * @param copybookFileFormat the copybookFileFormat to set
	 */
	public abstract Icb2xml2Xml setCopybookFileFormat(int copybookFileFormat);

//	/**
//	 * Old parameter, can be ignore most of the time
//	 * @param log the log to set
//	 */
//	public abstract ICobol2Xml setLog(AbsSSLogger log);
//
//	/**
//	 * whether to drop the copybook name from the start of the Field names. On the 
//	 * mainframe it is quite common to start (or end) a field name with the copybook name. This parameter
//	 * controls wether the this copybook name should be dropped or kept in JRecord.
//	 * <pre>
//	 * for copybook DTAR030:
//	 * 
//	 *     05 DTAR030.
//	 *        10 DTAR030-Product-Code            pic 9(8).
//	 *        10 DTAR030-Location                pic 9(4).
//	 *        05 DTAR030-Quantity                pic s9(6) comp-3.
//	 *        
//	 *  this option lets you remove/keep the copybook name at the start of the field name.
//	 * </pre>
//	 * 
//	 * @param dropCopybookNameFromFields drop the copybook name from the start of the Field names ?
//	 * In the above example if<ul>
//	 *   <li><b>true</b>  - DTAR030-Product-Code is converted to Product-Code.
//	 *   <li><b>false</b> - DTAR030-Product-Code remains unchanged.
//	 * </ul>
//	 * <br><b>Note: </b> The default is <b>false</b>
//	 */
//	public abstract ICobol2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * Normally the main element of the Xml is CobolData but you can
	 * set it to anything you want with this option.
	 * 
	 * @param xmlMainElement main tag name of the Xml
	 * 
	 * @return this item for "fluid" style assignment
	 */
	public Icb2xml2Xml setXmlMainElement(String xmlMainElement);
	
	/**
	 * Control wether the copybook name is dropped from the start of fields.
	 * If copybook is DTAR020 and the field is DTAR020-Keycode, then you might
	 * want to drop the DTAR020- from the start of the field name
	 * 
	 * @param dropCopybookNameFromFields true/false
	 * @return this item
	 */
	public Icb2xml2Xml setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	
	public void cobol2xml(String cobolFileName, String xmlFileName)  
			throws RecordException, IOException, JAXBException, XMLStreamException;
	
	public void cobol2xml(InputStream cobolStream, OutputStream xmlStream)
			throws RecordException, IOException, JAXBException, XMLStreamException;

	public void xml2Cobol(String xmlFileName, String cobolFileName)
			throws RecordException, IOException, JAXBException, XMLStreamException;

	public void xml2Cobol(InputStream xmlStream, OutputStream cobolStream)
			throws RecordException, IOException, JAXBException,
			XMLStreamException;



}
