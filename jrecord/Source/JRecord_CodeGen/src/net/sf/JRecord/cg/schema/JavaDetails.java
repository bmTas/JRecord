/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL
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
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.schema;

import net.sf.JRecord.cg.common.CCode;

public class JavaDetails {
	private final String cobolName, extensionName, javaName, className;

	protected JavaDetails(String cobolName) {
		super();
		
		StringBuilder b = CCode.cobolName2JavaName(cobolName);
		this.cobolName = cobolName;
	
		this.extensionName = CCode.toSuffix(b);
		this.javaName = CCode.toFieldName(b);
		this.className = CCode.toClassName(b);
	}

	/**
	 * @return the cobolName
	 */
	public final String getCobolName() {
		return cobolName;
	}

	/**
	 * @return the extensionName
	 */
	public final String getExtensionName() {
		return extensionName;
	}

	/**
	 * @return the javaName
	 */
	public final String getJavaName() {
		return javaName;
	}

	/**
	 * @return the className
	 */
	public final String getClassName() {
		return className;
	}
	
	
}
