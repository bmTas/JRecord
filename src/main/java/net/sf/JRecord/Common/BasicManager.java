
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Jean-Francois Gagnon
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2005, Bruce Martin / Jean-Francois Gagnon, All Rights Reserved.
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

/*
 * @Author Bruce Martin
 * Created on 28/08/2005
 *
 * Purpose: Act as a repository for type definitions
 *
 * Modification log:
 * On 2006/06/28 by Jean-Francois Gagnon:
 *    - Adjusted the type manager so it can manage the
 *      Fujitsu Zoned Decimal as well as the Sign Separate Numeric
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 *   - Added new Date and Checkbox types
 */
package net.sf.JRecord.Common;


/**
 * This class stores / returns managed classes to the calling program based
 * on a supplied index.
 *
 * i.e. it is a repository for variations on a class.
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class BasicManager<managedClass> {

    private final int invalidIndex;
    private final int systemEntries;
    private final int userRangeStart;

    private managedClass[] objects;

    private int userSize;
    private int used = 0;



    /**
     *  This class stores / retrieves variations on a Class
     *
     * @param numberOfSystemEntries the number of system entries
     * @param startOfUserRange where the user range starts
     * @param initialArray array of classes to be stored / requested by the user
     */
    public BasicManager(final int numberOfSystemEntries,
    				    final int startOfUserRange,
            		    final managedClass[] initialArray) {
        super();

        systemEntries = numberOfSystemEntries;
        invalidIndex  = systemEntries - 1;
        userSize  = initialArray.length - numberOfSystemEntries;
        userRangeStart = startOfUserRange;

        if (userSize < 0) {
            userSize = 0;
        }

        objects = initialArray;

    }



    /**
     * register a type & format
     *
     * @param classId type identifier of the type being top
     * @param obj object to be registered
     *
     */
    public void register(int classId, managedClass obj) {
        int idx = getIndex(classId);

        if (idx == invalidIndex) {
            throw new RecordRunTimeException(
            		Messages.INVALID_INDEX_MSG,
            		new Object[] {classId, userRangeStart, (userRangeStart + userSize)});
        }

        if (idx > used) {
        	used = idx;
        }

        objects[idx] = obj;
    }



    /**
     * Get a type Definition
     *
     * @param id type id to get the type definition for
     *
     * @return Type definition
     */
    public managedClass get(int id) {
        return objects[getIndex(id)];
    }


    /**
     * Adjusts the index
     *
     * @param key index supplied user
     *
     * @return adjusted index
     */
    protected final int getIndex(int key) {
        int idx = invalidIndex;

        if (key >= 0 && key < systemEntries) {
            idx = key;
        } else if (key >= userRangeStart
               &&  key <  userRangeStart + userSize) {
            idx =  key -  userRangeStart + systemEntries;
        }

        return idx;
    }


    /**
     * get key from an index
     *
     * @param index array index
     *
     * @return actual key
     */
    public int getKey(int index) {
    	int key = index;

    	if (key >= systemEntries) {
    		key = index - systemEntries + userRangeStart;
    	}

    	return key;
    }

    /**
     * Get the number of entries
     * @return The number of entries
     */
    public int getNumberOfEntries() {
        return objects.length;
    }


    public int getNumberUsed() {
        return used;
    }


	/**
	 * @return the userSize
	 */
	public int getUserSize() {
		return userSize;
	}
}
