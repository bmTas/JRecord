/*
 * @Author Bruce Martin
 * Created on 28/01/2007
 *
 * Purpose:
 * Convert an ExternalRecord into a LayoutDetail (internal form)
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

import net.sf.JRecord.Details.LayoutDetail;



/**
 * Convert an ExternalRecord (interface format) into a LayoutDetail (internal format)
 *
 * @author Bruce Martin
 *
 * @deprecated use externalRecord.asLayoutDetail() instead
 */
public class ToLayoutDetail {

    private static ToLayoutDetail instance = new ToLayoutDetail();

	/**
	 * convert an ExternalRecord into a LayoutDetail (internal form)
	 *
	 * @param externalRecord Standard record definition
	 *
	 * @return Group of records
	 * 
	 * @deprecated use externalRecord.asLayoutDetail() instead
	 */
	public LayoutDetail getLayout(ExternalRecord externalRecord) {

		if (externalRecord == null) {
			return null;
		}
		return externalRecord.asLayoutDetail();
	}


    /**
     * @return Returns the instance.
     */
    public static ToLayoutDetail getInstance() {
        return instance;
    }
}
