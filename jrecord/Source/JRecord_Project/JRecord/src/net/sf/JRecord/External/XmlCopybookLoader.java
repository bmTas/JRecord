/*
 * @Author Bruce Martin
 * Created on 6/04/2005
 *
 * Purpose:
 * This class will insert a XML Copybook into the Record Editor DB
 *
 * Modification log:
 * Changes
 * # Version 0.56
 *   - On 2006/06/28 by Jean-Francois Gagnon:
 *    - Added the ability to process copybooks so they get
 *      processed with the Fujitsu Flavor
 *    - Added handling of the Sign Separate type field
 *   - Bruce Martin 2007/01/16
 *     - Changed to use ExtendedRecord to get null records
 *
 * # Version 0.61
 *   - fixed bug of not calculating decimal places correctly
 *     when lower case v used in Cobol Format
 *
 *  # Version 0.61b
 *   - Strip directories when / is used instead of \
 */
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

package net.sf.JRecord.External;

import net.sf.JRecord.External.base.BaseCb2xmlLoader;

/**
 * This class will load a cb2xml XML Copybook into the Record Layout
 *
 * @author Bruce Martin
 */
public class XmlCopybookLoader extends BaseCb2xmlLoader<ExternalRecord> 
implements ICopybookLoaderStream, ICopybookLoaderCobol {

    public XmlCopybookLoader() {
		super(new ExternalRecordBuilder(), true);
	}
}