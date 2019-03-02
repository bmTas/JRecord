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

import java.util.TreeMap;

public class Options {

	public static final IRecordPositionOption RP_FIRST_RECORD_IN_FILE = new OptionType("First");
	public static final IRecordPositionOption RP_MIDDLE_RECORDS       = new OptionType("Middle");
	public static final IRecordPositionOption RP_LAST_RECORD_IN_FILE  = new OptionType("Last");
	
	public static final OptionMap<IRecordPositionOption> recordPositionMap = new OptionMap<IRecordPositionOption>(
			RP_FIRST_RECORD_IN_FILE, RP_MIDDLE_RECORDS, RP_LAST_RECORD_IN_FILE
			);
	
	
	public static class  OptionMap<OT extends IOptionType> {
		final TreeMap<String, OT> map = new TreeMap<String, OT>();
		
		@SafeVarargs
		private OptionMap(OT...iOptionTypes) {
			for (OT t : iOptionTypes) {
				map.put(t.toString().toLowerCase(), t);
			}
		}
		
		public IOptionType get(String name) {
			if (name == null) {
				return null;
			}
			
			return map.get(name.toLowerCase());
		}
	}
}
