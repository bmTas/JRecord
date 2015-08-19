package net.sf.JRecord.def.IO.builders;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;


/**
 * <p>This interface is for creating CSV-Readers and writers</p>
 * 
 * <pre>
 * You can create a Csv-Reader by:
 * 
 *     AbstractLineReader reader = JRecordInterface1.CSV
 *             .newIOBuilder("    ", "\"")
 *                     .newReader(salesFile);
 *                     
 * and a Csv Writer by:
 * 
 *     ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV
 *             .newIOBuilder(";", "\"")
 *                     .defineFields()
 *                         .addCsvField(FLD_SKU,   Type.ftChar, 0)
 *                         .addCsvField(FLD_STORE, Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_DATE,  Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_QTY,   Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_PRICE, Type.ftNumAnyDecimal, 0)
 *                         .addCsvField(FLD_GST,   Type.ftNumAnyDecimal, 0)
 *                     .endOfRecord();
 *   </pre>
 * 
 * @author Bruce Martin
 *
 */
public interface ICsvIOBuilder extends IIOBuilder {

	/**
	 * File Organization or File Structure (e.g. VB, Fixed Width Etc. Use Constants.IO_*
	 * The main options are:<ul>
	 *   <li><b>Constants.IO_STANDARD_TEXT_FILE</b> - Standard Windows/*nix/Mac text file using  \n, \n\r etc as a record (or line) delimiter.
	 *   Used for Csv files with-out names on the first-
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
	public abstract ICsvIOBuilder setFileOrganization(int fileOrganization);
	


	/**
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract ICsvIOBuilder setFont(String font);

	

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
	public abstract ICsvIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);


	/*
	 * 	 * <b><font color="red">Warning</font> This option has not been tested yet !!!</b>
	 *

	 */
	public abstract IDefineCsvFields defineFields();


	/**
	 * Set the Csv delimiter or field separator
	 * @param val new field delimiter or Field separator
	 * @return this ICsvIOBuilder
	 */
	public abstract ICsvIOBuilder setDelimiter(String val);


	/**
	 * Set the Quote character to be used 
	 * @param val quote value
	 * @return this ICsvIOBuilder
	 */
	public abstract ICsvIOBuilder setQuote(String val);
}
