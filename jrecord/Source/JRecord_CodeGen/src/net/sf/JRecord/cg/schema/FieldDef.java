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

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.cg.common.CCode;

/**
 * Class to describe one field in the file. It is used for Code generation 
 * @author Bruce Martin
 *
 */
public class FieldDef extends JavaDetails {
	private final FieldDetail fieldDetail;
	private final ArrayElement arrayDetails;
	public final String javaType;


	public FieldDef(String cobolName, FieldDetail fieldDef, ArrayElement ai) {
		super(cobolName);
		this.fieldDetail = fieldDef;
		this.arrayDetails = ai;
		this.javaType = CCode.typeToJavaType(fieldDetail.getType(), fieldDetail.getLen(), fieldDetail.getDecimal());
	}

	/**
	 * @return the fieldDef
	 */
	public final FieldDetail getFieldDetail() {
		return fieldDetail;
	}
	
	/**
	 * @return the arrayDetails
	 */
	public final ArrayElement getArrayDetails() {
		return arrayDetails;
	}
	
	public boolean isArrayItem() {
		return arrayDetails != null;
	}
	
	public boolean alwaysIncludeField() {
		return arrayDetails == null || arrayDetails.isSpecial();
	}

	public final String getJRecordTypeId() {
		return CCode.getJRecordTypeName(fieldDetail.getType());
	}

	/**
	 * @return the javaType
	 */
	public final String getJavaType() {
		return javaType;
	}

	public final String getAsType() {
		return "as" + javaType.substring(0, 1).toUpperCase() + javaType.substring(1);
	}

	public final String getFieldInitialise() {
		String ret = "0";
		if ("String".equals(javaType)) {
			ret = "\"\"";
		} else if (javaType.startsWith("Big")) {
			ret = javaType + ".ZERO";
		}
		return ret;
	}

}
