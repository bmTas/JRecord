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
      
package net.sf.JRecord.Log;

import java.io.IOException;

public final class CommonCode {

	public static void append(Appendable buf, Exception ex) {
		
		StackTraceElement[] el = ex.getStackTrace();
		int fin = Math.min(1000, el.length);

		try {
			buf.append("\n\nClass=");
			buf.append(ex.getClass().getName());
			buf.append("    Error=");
			buf.append(ex.getMessage());
			buf.append("\n");

			for (int i = 0; i < fin; i++) {
				buf.append("\n    ");
				buf.append(el[i].getClassName());
				buf.append("  :  ");
				buf.append(Integer.toString(el[i].getLineNumber()));
			}
		} catch (IOException e) {
			printAppendError(e, ex);
		}

	}
	
	public static void printAppendError(Exception e, Exception ex) {
		System.err.println();
		System.err.println("Error in Appending: ");
		e.printStackTrace();
		if (ex != null) {
			System.err.println();
			System.err.println("Caused by: ");
			ex.printStackTrace();
		}

	}
}
