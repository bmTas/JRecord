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

package net.sf.JRecord.Details;

/**
 * 
 * Cobol Copybooks do not provide a means to determine which
 * Record applies to a particular Data-Line. Most of the
 * time this does not matter but there are exceptions 
 * (e.g. <b>Constants.IO_CONTINOUS_NO_LINE_MARKER</b>).
 * 
 * <p>One way to tell
 * JRecord / RecordEditor which Record to use for a Data-line
 * is to define a {@link RecordDecider}.
 * 
 * <p>This .
 * 
 * <pre>
 *   iobuilder.setRecordDecider(myRecordDecider);
 * </pre>
 * 
 *
 * @author Bruce Martin
 *
 */

public interface IRecordDeciderX extends RecordDecider {

	/**
	 * Define the <i>Layout</i> to the {@link RecordDecider}.
	 * This method is called by the LayoutDetail class.
	 * <p>This allows for better performing RecordDeciders.
	 * 
	 *  <p><b>For internal JRecord use !!!</b>
	 * 	 * 
	 * @param layout that uses the RecordDecider
	 */
	public abstract void setLayout(LayoutDetail layout);
}
