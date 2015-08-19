package net.sf.JRecord.def.IO.builders;

import java.io.IOException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;


/**
 * Basic Interface to define IOBuilders
 * 
 * @author Bruce Martin
 *
 */
public interface IIOBuilder extends ISchemaIOBuilder{


	
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
	public abstract IIOBuilder setFileOrganization(int fileOrganization);

	/**
	 * Set the character-set (font)
	 * 
	 * @param font the font (or character set) of the File e.g CP037 is US-EBCDIC, CP273 is German EBCDIC
	 */
	public abstract IIOBuilder setFont(String font);

	

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
	public abstract IIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);


	
	/**
	 * Get the ExternalRecord (Schema-Builder) class
	 *  
	 * @return ExternalRecord (Schema-Builder) class 
	 * 
	 * @throws RecordException
	 * @throws IOException
	 */
	public abstract ExternalRecord getExternalRecord() throws RecordException,
			IOException;

	
	

}