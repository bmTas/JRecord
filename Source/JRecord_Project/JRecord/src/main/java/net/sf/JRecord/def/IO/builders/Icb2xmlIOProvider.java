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

/**
 * Interface to create CobolIOBuilders (a Builder builder).
 * <pre>{@code
 *      AbstractLineReader r = JRecordInterface1.CB2XML
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 * }</pre> 

 * @author Bruce Martin
 *
 */
public interface Icb2xmlIOProvider {

	/**
	 * Create a new Cobol IOBulder from a COBOL-Copybook file
	 * 
	 *<pre>
	 *<b>Example:</b>
	 * 
	 *      AbstractLineReader r = JRecordInterface1.CB2XML
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
	 *
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
	public abstract Icb2xmlIOBuilder newIOBuilder(
			String copybookFileame);

	/**
	 * Create a new Cobol IOBulder for a file.
	 * 
	 *<pre>
	 *<b>Example:</b>
	 *       
	 *      AbstractLineReader r = JRecordInterface1.CB2XML
	 *             .<b>newIOBuilder(cobolCopybookStream, "My-Cobol-Record")</b>
	 *                 .setFileOrganization(Constants.IO_FIXED_LENGTH)
	 *                 .setDialect(ICopybookDialects.FMT_FUJITSU)
	 *             .newReader("Data-Filename");
	 * </pre> 
     *
     * @param cobolCopybookStream stream to read the Cobol Copybook from.
	 * @param copybookName name of the Copybook (or schema file).
	 * 
	 * These are the default values (which can be over-ridden with the appropriate set* method)
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
	public abstract Icb2xmlIOBuilder newIOBuilder(
			InputStream cobolCopybookStream, String copybookName);

	/**
	 * This method returns that will combine multiple Cobol Copybooks into the one internal copybook or File-Schema
	 * @param copybookname name of the copybook
	 * @return requested IOBuilder
	 */
	public abstract Icb2xmlMultiFileIOBuilder newMultiCopybookIOBuilder(String copybookname);

}