/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.def.IO.builders;

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Option.IRecordPositionOption;


/**
 * These classes will create <i>Cobol Record</i> Readers/Writers using the supplied COBOL copybook
 * 
 *<pre>
 *<b>Example:</b>
 * 
 *      {@code
 *      AbstractLineReader r = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 * }</pre> 
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
 * @author Bruce Martin
 *
 */
public interface Icb2xmlIOBuilder extends IIOBuilder, Icb2xmlLoadOptions {

	@Override public abstract Icb2xmlIOBuilder setFileOrganization(int fileOrganization);

	@Override  public abstract Icb2xmlIOBuilder setSplitCopybook(int splitCopybook);

	@Override public abstract Icb2xmlIOBuilder setFont(String font);

	@Override public abstract Icb2xmlIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	@Override public abstract Icb2xmlIOBuilder setRecordPositionCode(String recordName, IRecordPositionOption positionOption);
	
	@Override public abstract Icb2xmlIOBuilder setRecordParent(String recordName, String parentName);
	
	@Override public abstract Icb2xmlIOBuilder setRecordDecider(RecordDecider recordDecider);

	/**
	 * Old parameter, can be ignore most of the time
	 * @param log the log to set
	 */
	public abstract Icb2xmlIOBuilder setLog(AbsSSLogger log);

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
	public abstract Icb2xmlIOBuilder setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);
	
	/**
	 * Controls wether fillers are kept in the layout.
	 * 
	 * 
	 * @param keepFillers keep fillers in the layout
	 * @return this for further updates
	 * 
	 * @deprecated While you can use this option, I suggest
	 * you try and avoid it (i.e. name the filler fields).
	 */
	public abstract Icb2xmlIOBuilder setKeepFillers(boolean keepFillers);

}