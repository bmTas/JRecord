/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
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
      
package net.sf.JRecord.Option;


/**
 * Options to split a Cobol Copybook into seperate records
 * 
 * @author Bruce Martin
 *
 */
public interface ICobolSplitOptions {
	
	/** Standard Single record copybook */
    public static final int SPLIT_NONE     = 0;
    /** 
     *  Multi-Record Copybook with each record is in a redefines
     *  <pre>
     *     01  Trans-File.
     *         .....
     *        03 Record-Type                  Pic X.
     *        ....
     *        03  Header-Record.
     *            ..... 
     *        03  Detail-Record redefines Header-Record.
     *  </pre>
     */
    public static final int SPLIT_REDEFINE = 1;
    /** 
     *  Multi-Record Copybook with each record on a 01 Group level
     *  <pre>
     *     01  Header-Record.
     *         .....
     *     01  Detail-Record.
     *  </pre>
     */
    public static final int SPLIT_01_LEVEL = 2;
    /** 
     *  Multi-Record Copybook with each record in a Group level under 01 
     *  i.e. 05 in the following 
     *  <pre>
     *        05  Header-Record.
     *            .....
     *        05  Detail-Record.
     *  </pre>
     */
    public static final int SPLIT_TOP_LEVEL = 3;
    /** 
     *  Multi-Record Copybook with each record in a Group level under 01 
     *  i.e. 05 in the following 
     *  <pre>
     *        01  TOP-LEVEL
     *            05  Header-Record.
     *                .....
     *            05  Detail-Record.
     *  </pre>
     */
    public static final int SPLIT_HIGHEST_REPEATING = 4;

}
