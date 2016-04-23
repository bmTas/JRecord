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
      
package net.sf.JRecord.Common;

/**
 * Abstract Manager - Interface of a class that manages other classes
 *
 *
 * @author Bruce Martin
 *
 */
public interface AbstractManager {


    /**
     * Get the number of entries
     * @return The number of entries
     */
    public int getNumberOfEntries();

    /**
     * Get the name of the manager
     * @return name of the manager
     */
    public String getManagerName();

	/**
	 * get key (from the index)
	 * @param idx get key for index number
	 * @return the key value for the index
	 */
	public abstract int getKey(int idx);

	/**
	 * get the name of managed class (from the inex)
	 * @param idx get name for index number
	 * @return the name for the index
	 */
	public abstract String getName(int idx);

}