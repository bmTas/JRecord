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

import java.util.Properties;

public class UserInit {

	static {

		Properties properties = PropertyManager.getProperties();
		String init = "init.";

		String var, className;
		Object o;
		@SuppressWarnings("rawtypes")
		Class c;

//		if (properties != null) {
			for (int i = 0; i < 32; i++) {
				var = init + i;
				if (properties.containsKey(var)) {
					try {
						className = properties.getProperty(var);
						c = Class.forName(className);
						if (c != null) {
							o = c.newInstance();

							if (o instanceof Runnable) {
								((Runnable) o).run();
							}
						}
					} catch (Exception e) {
						// TODO: handle exception
					}
				}
			}
//		}
	}

	public static void init() {

	}
}
