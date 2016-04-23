/*
 * @Author Bruce Martin
 * Created on 10/09/2005
 *
 * Purpose:
 *   RecordDecider's are used decide what RecordDetail
 * should be used to display a line (or record)
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

package net.sf.JRecord.Details;


/**
 * RecordDecider's are used decide which specific RecordDetail
 * should be used to format a line (or data record). It allow
 * you to write Java Code to decide which particular Record Should Be used.
 *
 * @author Bruce Martin
 *
 */
public interface RecordDecider {

    /**
     * Get the preferred Layout
     *
     * @param line to decide what the preferred layout is
     *
     * @return the preferred layout
     */
    public abstract int getPreferedIndex(AbstractLine line);
}
