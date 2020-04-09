/**
 * This package holds examples of reading in a Cobol Copybook, updating the copybook
 * and writing the Copybook out as a Xml-Copybook (or schema). 
 * 
 * <p>The uses for this process include:
 * <ul>
 *   <li>Create a <i>matching</i> Csv schema for use in utility copy to Csv from Cobol
 *   or for use in writing the Csv file (using JRecord) that will be copies to Cobol.  
 *   <li>Rename Duplicate field names in the Cobol copybook. This can be useful in generic
 *   processing.
 *   <li>Adding extra information (e.g. FileStructure of Record-Parent information to the Cobol Copybook)
 *   and then writing it out as a Xml-Copybook (or schema)
 * </ul> 
 *   
 */
/**
 * @author Bruce Martin
 *
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.recordEditorXml.manipulate;