package net.sf.JRecord.def.IO.builders;

import java.io.InputStream;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Option.IRecordPositionOption;

public interface ICobolMultiCopybookIOBuilder extends  ICobolIOBuilder {

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
	public abstract ICobolMultiCopybookIOBuilder setFileOrganization(int fileOrganization);

	/**
	 * @param splitCopybook Wether to Split the Copybook or not. Options are:<ul>
	 *    <li><b>CopybookLoader.SPLIT_NONE</b> - No Split
	 *    <li><b>CopybookLoader.SPLIT_REDEFINE</b> - Split on highest-Level redefines
	 *    <li><b>CopybookLoader.SPLIT_01_LEVEL</b> - Split on 01 levels
	 *    <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> - Split on the highest group level
	 *    in the copybook.
	 * </ul>
	 * <p>Most of the time it does not matter what this parameter is set to, exceptions  
	 * include copybook where there are multiple-records at a group level > 01 (use SPLIT_HIGHEST_REPEATING):
	 * <pre>
	 * 
	 *         05  Header-Record.
	 *             ....
	 *         05  Detail-Record.
	 *             ....
	 *         05  Trailer-Record.
	 *             ....
	 * </pre>
	 */
	public abstract ICobolMultiCopybookIOBuilder setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */
	public abstract ICobolMultiCopybookIOBuilder setDialect(int dialect);


	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract ICobolMultiCopybookIOBuilder setFont(String font);

	

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
	public abstract ICobolMultiCopybookIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);
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
	public abstract ICobolMultiCopybookIOBuilder setCopybookFileFormat(int copybookFileFormat);

	/**
	 * Old parameter, can be ignore most of the time
	 * @param log the log to set
	 */
	public abstract ICobolMultiCopybookIOBuilder setLog(AbsSSLogger log);

	/**
	 * whether to drop the copybook name from the start of the Field names. On the 
	 * mainframe it is quite common to start (or end) a field name with the copybook name. This parameter
	 * controls wether the this copybook name should be dropped or kept in JRecord.
	 * <pre>
	 * for copybook DTAR030:
	 * 
	 *     05 DTAR030.
	 *        10 DTAR030-Product-Code            pic 9(8).
	 *        10 DTAR030-Location                pic 9(4).
	 *        05 DTAR030-Quantity                pic s9(6) comp-3.
	 *        
	 *  this option lets you remove/keep the copybook name at the start of the field name.
	 * </pre>
	 * 
	 * @param dropCopybookNameFromFields drop the copybook name from the start of the Field names ?
	 * In the above example if<ul>
	 *   <li><b>true</b>  - DTAR030-Product-Code is converted to Product-Code.
	 *   <li><b>false</b> - DTAR030-Product-Code remains unchanged.
	 * </ul>
	 * <br><b>Note: </b> The default is <b>false</b>
	 */
	public abstract ICobolMultiCopybookIOBuilder setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * Add another copybook to be imported
	 *  
	 * @param fileName
	 * 
	 * @return
	 */
	public abstract ICobolMultiCopybookIOBuilder addCopyBook(String fileName);
	
	/**
	 * Add a copybook stream to the builder
	 * 
	 * @param inStream input stream
	 * @param copybookName copybook name
	 * 
	 * @return this IOBuilder
	 */
	public abstract ICobolMultiCopybookIOBuilder addCopyBook(InputStream inStream,
			String copybookName);

	/**
	 * Define the record Selection for the last copybook defined
	 * @param recordSelection record selection to be used
	 * @return IOBuilder for further definition (fluid style)
	 */
	public abstract ICobolMultiCopybookIOBuilder setRecordSelectionCurrentCopybook(	ExternalSelection recordSelection);

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
	public abstract ICobolMultiCopybookIOBuilder setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption);

}
