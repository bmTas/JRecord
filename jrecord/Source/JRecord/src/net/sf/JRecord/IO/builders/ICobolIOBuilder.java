package net.sf.JRecord.IO.builders;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Log.AbsSSLogger;


/**
 * These classes will read Cobol-Copybooks and create <i>Cobol Record</i> Readers/Writers
 * 
 * @author Bruce Martin
 *
 */
public interface ICobolIOBuilder {

	/**
	 * 
	 * @return the layout or File-Schema (File Description)
	 */
	public abstract LayoutDetail getLayout() throws RecordException, IOException;

	/**
	 * @param splitCopybook Wether to Split the Copybook or not. Options are:<ul>
	 *    <li><b>CopybookLoader.SPLIT_NONE</b> - No Split
	 *    <li><b>CopybookLoader.SPLIT_REDEFINE</b> - Split on highest-Level redefines
	 *    <li><b>CopybookLoader.SPLIT_01_LEVEL</b> - Split on 01 levels
	 *    <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> - Split on the highest group level
	 *    in the copybook.
	 * </ul>
	 * <p>Most of the time it does not matter what this parameter is set to, the exception is 
	 * if you have copybook if there are multiple-records at a group level > 01:
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
	public abstract ICobolIOBuilder setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul
	 * @param dialect new Cobol Dialect
	 */
	public abstract ICobolIOBuilder setDialect(int dialect);
	
	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract ICobolIOBuilder setFont(String font);

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
	public abstract ICobolIOBuilder setCopybookFileFormat(int copybookFileFormat);

	/**
	 * Old parameter, can be ignore most of the time
	 * @param log the log to set
	 */
	public abstract ICobolIOBuilder setLog(AbsSSLogger log);

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
	 *        05 DTAR020-Quantity                pic s9(6) comp-3.
	 * </pre>
	 * 
	 * @param dropCopybookNameFromFields drop the copybook name from the start of the Field names ?
	 */
	public abstract ICobolIOBuilder setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
	 * The main options are:<ul>
	 *   <li><b>Constants.IO_DEFAULT</b> - JRecord will decide the actual method based on other values (The Default Value).
	 *   It is generally better to explicitly set the File-Organisation (or file-Structure).
	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard Windows/*nix/Mac text file using  \n, \n\r etc as a record (or line) delimiter.
	 *   <li><b>Constants.IO_UNICODE_TEXT</b> - Standard Windows/*nix/Mac Unicode / double byte text file using  \n, \n\r etc 
	 *   as a record (or line) delimiter. It ensures record are stored in character format (instead of bytes).
	 *   <li><b>Constants.IO_FIXED_LENGTH</b> - Every Record (or line) is a standard Fixed length based on the Maximum
	 *   schema length.
	 *   <li><b>Constants.IO_FIXED_LENGTH_CHAR</b> - Fixed length character file (typically used for Fixed-Length unicode files).
	 *   <li><b>Constants.IO_VB</b> - Mainframe VB (Variable Record length file). Records consist of a Record-Length followed by the Record-Data.
	 *   <li><b>Constants.IO_VB_DUMP</b> - Raw Block format of a Mainframe-VB file. You get this format  if you specify RECFM=U when reading it on the mainframe.
	 *   <li><b>Constants.IO_VB_OPEN_COBOL</b> - GNU (open-Cobol) VB format.
	 * </ul>
	 * 
	 * @param fileOrganization File Organization (or File Structure)
	 */
	public abstract ICobolIOBuilder setFileOrganization(int fileOrganization);

	/**
	 * Get the ExternalRecord (Schema-Builder) class 
	 * @return ExternalRecord (Schema-Builder) class 
	 * @throws RecordException
	 * @throws IOException
	 */
	public abstract ExternalRecord getExternalRecord()  
			throws RecordException, IOException;

	/**
	 * Get a new Line
	 * @return Line
	 */
	public abstract AbstractLine newLine() throws IOException, RecordException;
	
	
	/**
	 * Create a new LineReader for a specified file
	 * 
	 * @param filename name of the file to create the reader for
	 * @return Requested LineReader
	 * @throws FileNotFoundException
	 * @throws IOException anyIoexception that occurs
	 * @throws RecordException any JRecord
	 */
	public abstract AbstractLineReader newReader(String filename)
			throws FileNotFoundException, IOException, RecordException;

	/**
	 * Create a new LineReader for a supplied input stream
	 * @param datastream input datastream
	 * @return Requested LineReader
	 * @throws IOException 
	 * @throws RecordException
	 */
	public abstract AbstractLineReader newReader(InputStream datastream)
			throws IOException, RecordException;

	/**
	 * Create LineWriter for a supplied filename
	 * @param filename output filename
	 * @return Requested LineWriter
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws RecordException
	 */
	public abstract AbstractLineWriter newWriter(String filename)
			throws FileNotFoundException, IOException, RecordException;

	/**
	 * Create LineWriter for a supplied stream
	 * @param datastream output stream where the file is going to be written
	 * @return the Requested
	 * 
	 * @throws IOException
	 * @throws RecordException
	 */
	public abstract AbstractLineWriter newWriter(OutputStream datastream)
			throws IOException, RecordException;


}