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
 * Class to manage other classes (with a name given to the managed classes).
 * Also used to build combo box's etc in the RecordEditor
 *
 * @author Bruce Martin
 *
 * @param <managedClass> class being managed
 */
public abstract class BasicNamedManager<managedClass>
   extends BasicManager<managedClass>
implements AbstractManager {

	private String[] names;
	private String mgrName;

	/**
	 * Create a Named Manager
	 * @param managerName name of the "Manager"
	 * @param numberOfSystemEntries number of System Entries
	 * @param startOfUserRange first user key
	 * @param initialArray array of classes to be returned to the user
	 */
	public BasicNamedManager(String managerName, int numberOfSystemEntries, int startOfUserRange, final managedClass[] initialArray) {
		super(numberOfSystemEntries, startOfUserRange, initialArray);

		names = new String[super.getNumberOfEntries()];
		mgrName = managerName;
	}

    /**
	 * @see net.sf.JRecord.Common.AbstractManager#getManagerName()
	 */
	@Override
	public String getManagerName() {
		return mgrName;
	}

	/**
	 * Register a Class
	 *
	 * @see BasicManager#register(int, Object)
	 *
	 * @deprecated use register(id, name, parser)
	 */
	public final void register(int id, managedClass parser) {
		register(id, "",  parser);
	}

	/**
	 * Register a new class with a name
	 * @param id Identifier of Class
	 * @param name User name of the class
	 * @param parser new parser
	 */
	public void register(int id, String name, managedClass parser) {
		super.register(id, parser);
		names[id] = name;
	}

	/**
	 * @see net.sf.JRecord.Common.AbstractManager#getName(int)
	 */
	public String getName(int id) {
		return names[id];
	}


}
