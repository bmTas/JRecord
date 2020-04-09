/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 3 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 3.0 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.schema;

import net.sf.JRecord.fieldNameConversion.FieldNameConversionManager;
import net.sf.JRecord.fieldNameConversion.IFieldNameConversion;

//import net.sf.JRecord.cg.common.CCode;

public class JavaDetails {
	private final String cobolName,  extensionName, javaName, className, constantName, sqlName,
			standardisedName;

	protected JavaDetails(String cobolName, String copybookName, String classname) {
		super();
		
		IFieldNameConversion conversion = FieldNameConversionManager.getCurrentConversion();


		String adjCobolName = conversion.toAdjCobolName(cobolName, copybookName);
//		String adjCobolName = cobolName==null? "" :cobolName;
//		
//		if (copybookName != null && copybookName.length() > 0) {
//			String lcAdjCobolName = adjCobolName.toLowerCase();
//			String lcCopybookName = copybookName.toLowerCase();
//			if (lcAdjCobolName.length() > lcCopybookName.length() && lcAdjCobolName.startsWith(lcCopybookName)) {
//				adjCobolName = adjCobolName.substring(copybookName.length());
//				if (lcAdjCobolName.startsWith("-") || lcAdjCobolName.startsWith("_")) {
//					adjCobolName = adjCobolName.substring(1);
//				}
//			} 
//		}
		
		
		String name = conversion.cobolName2JavaName(adjCobolName);
		this.cobolName = cobolName;

		if (classname == null) {
			this.extensionName = conversion.toSuffix(name);			
			this.className = conversion.toClassName(name);
		} else {
			this.extensionName = conversion.toSuffix(classname);			
			this.className = extensionName;
		}
		this.javaName = conversion.toFieldName(name);
		this.constantName = conversion.toConstant(adjCobolName)
				;
		this.sqlName = conversion.toSqlName(adjCobolName);
		this.standardisedName = conversion.string2JavaId(adjCobolName);
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

 
	/**
	 * @return the constantName
	 */
	public final String getConstantName() {
		return constantName;
	}

	/**
	 * @return the sqlName
	 */
	public final String getSqlName() {
		return sqlName;
	}

	public String getStandardisedName() {
		return standardisedName;
	}
	
	
}
