/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: RecordEditor's version of JRecord 
 *    
 *    Sub-Project purpose: Low-level IO and record translation  
 *                        code + Cobol Copybook Translation
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 2.1 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.External.base;

import java.io.OutputStream;

import net.sf.JRecord.Log.AbsSSLogger;


/**
 * Description of a Class to write a copybook (Record Layout or Record Description) 
 * to a file
 * 
 * @author Bruce Martin
 *
 */
public interface CopybookWriter {

	/**
	 * Write a copybook to a file
	 * @param directory Directory to write copybooks
	 * @param copybook Copybook Details
	 * @param log log of errors that occur
	 * @throws Exception any error that occurs
	 */
	
	public abstract String writeCopyBook(final String directory, BaseExternalRecord<?> copybook,
            final AbsSSLogger log) throws Exception;
	
	/**
	 * Write Copybook to a Stream
	 * @param outStream stream where the copybook should be written
	 * @param copybook copybook to be written
	 * @param log log for any errors to be written
	 * @throws Exception any errors thrown
	 */
	public abstract void writeCopyBook(final OutputStream outStream, BaseExternalRecord<?> copybook,
            final AbsSSLogger log) throws Exception;

}
