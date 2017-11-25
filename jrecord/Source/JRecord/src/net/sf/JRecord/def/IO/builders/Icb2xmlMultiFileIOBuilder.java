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

import java.io.InputStream;

import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Option.IRecordPositionOption;

/**
 * These classes will create <i>Cobol Record</i> Readers/Writers using the supplied cb2xml copybook
 * 
 *<pre>
 *<b>Example:</b>
 * 
 *      {@code
 *      AbstractLineReader r = JRecordInterface1.CB2XML
 *              .newMultiCopybookIOBuilder()
 *                      .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .addCopyBook("copybook1.xml")
 *                  .addCopyBook("copybook2.xml")
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
 *  <li><b>setCopybookFileFormat</b> - is a standard Column 6-72 or some other format
 *  <li><b>setFont</b> Set the font (character-set) used in the Data-File.
 *  </ul>
 *
 * @author Bruce Martin
 *
 */
public interface Icb2xmlMultiFileIOBuilder extends  Icb2xmlIOBuilder {

	/*
	
	/**
	 * {@inheritDoc}
	 */
	@Override public abstract Icb2xmlMultiFileIOBuilder setFileOrganization(int fileOrganization);

	@Override public abstract Icb2xmlMultiFileIOBuilder setSplitCopybook(int splitCopybook);

	@Override public abstract Icb2xmlMultiFileIOBuilder setFont(String font);

	@Override public abstract Icb2xmlMultiFileIOBuilder setRecordSelection(String recordName, ExternalSelection selectionCriteria);
	
	@Override public abstract Icb2xmlMultiFileIOBuilder setRecordParent(String recordName, String parentName);

	@Override public abstract Icb2xmlMultiFileIOBuilder setLog(AbsSSLogger log);

	@Override public abstract Icb2xmlMultiFileIOBuilder setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);
	
	@Override public abstract Icb2xmlMultiFileIOBuilder setInitToSpaces(boolean initToSpaces);
	
	@Override public abstract Icb2xmlMultiFileIOBuilder setRecordDecider(RecordDecider recordDecider);


	/**
	 * Add another copybook to be imported
	 *  
	 * @param fileName
	 * 
	 * @return this IOBuilder (for more updates)
	 */
	public abstract Icb2xmlMultiFileIOBuilder addCopyBook(String fileName);
	
	/**
	 * Add a copybook stream to the builder
	 * 
	 * @param inStream input stream
	 * @param copybookName copybook name
	 * 
	 * @return this IOBuilder
	 */
	public abstract Icb2xmlMultiFileIOBuilder addCopyBook(InputStream inStream,
			String copybookName);

	/**
	 * Define the record Selection for the last copybook defined
	 * @param recordSelection record selection to be used
	 * @return IOBuilder for further definition (fluid style)
	 */
	public abstract Icb2xmlMultiFileIOBuilder setRecordSelectionCurrentCopybook(	ExternalSelection recordSelection);


	@Override public abstract Icb2xmlMultiFileIOBuilder setRecordPositionCode(
			String recordName, IRecordPositionOption positionOption);

	/**
	 * Set the starting position for current copybook
	 * @param position starting position
	 * @return IOBuilder for further definition (fluid style)
	 */
	public abstract Icb2xmlMultiFileIOBuilder setStartingPosition(int position) ;

	/**
	 * Set the starting position relative to a field
	 * @param recordName Record holding the field
	 * @param fieldName name of the field
	 * @return IOBuilder for further definition (fluid style)
	 */
	public abstract Icb2xmlMultiFileIOBuilder setStartingPositionToField(String recordName, String fieldName);

}
