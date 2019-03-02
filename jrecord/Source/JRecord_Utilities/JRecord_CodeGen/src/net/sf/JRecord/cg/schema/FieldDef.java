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

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.cg.schema.classDefinitions.IClassDef;
import net.sf.JRecord.cgen.support.Code2JRecordConstants;

/**
 * Class to describe one field in the file. It is used for Code generation 
 * @author Bruce Martin
 *
 */
public class FieldDef extends JavaDetails {
	private final IFieldDetail fieldDetail;
	private final ArrayElement arrayDetails;
	private CobolItemDef cobolItemDef;
	public final String javaType, javaRawType, jrecAs;
	private String value = null;
	public final boolean shortNumber;
	public final IClassDef classDef;
	


	public FieldDef(String cobolName, FieldDetail fieldDef, ArrayElement ai, String schemaName,
			boolean isCsv) {
		super(cobolName, schemaName, null);
		int type = fieldDef.getType();
		JavaTypeDetails td = new JavaTypeDetails(isCsv, fieldDef);

		this.fieldDetail = fieldDef;
		this.arrayDetails = ai;
		this.classDef = td.classDef;
		this.javaRawType = td.javaRawType;
		this.javaType = td.javaType;
		this.jrecAs = "as" + (classDef == null || classDef.getJrecAs() == null 
				? "short".equals(javaType) 
						? "Int" 
						: javaType.substring(0, 1).toUpperCase() + javaType.substring(1)
				: classDef.getJrecAs());

		shortNumber = TypeManager.getInstance().getShortType(type, fieldDef.getLen(), fieldDef.getFontName()) != type;
	}

	/**
	 * @return the fieldDef
	 */
	public final IFieldDetail getFieldDetail() {
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
		return Code2JRecordConstants.getJRecordTypeName(fieldDetail.getType());
	}

	/**
	 * @return the javaType
	 */
	public final String getJavaType() {
		return  javaType ;
	}
	/**
	 * @return the javaType
	 */
	public final String getRawJavaType() {
		return javaRawType;
	}

	public final String getAsType() {
		return jrecAs;
	}
	
	public final String formatGet(String value) {
		
		if (classDef != null) {
			return classDef.generateFromPojo(value);
		}
		return value;
	}

	
	public final String formatSet(String value) {
		if (classDef != null) {
			return classDef.generateToPojo(value);
		}
		return "short".equalsIgnoreCase(javaType) ? "(short) " + value : value;
	}


	/**
	 * @return the value
	 */
	public final String getValue() {
		return value;
	}

	/**
	 * @param value the value to set
	 */
	public final void setValue(String value) {
		this.value = value;
	}
	
	/**
	 * 
	 * @return is it a primitive (short / int / long / double) numeric type 
	 */
	public final boolean isPrimitiveNumeric() {
		return TypeManager.isNumeric(fieldDetail.getType()) && fieldDetail.getDecimal() == 0;
	}

	public final boolean isNumeric() {
		return TypeManager.isNumeric(fieldDetail.getType());
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

	/**
	 * @return the shortNumber
	 */
	public boolean isShortNumber() {
		return shortNumber;
	}

	/**
	 * @return the cobolItemDef
	 */
	public CobolItemDef getCobolItemDef() {
		return cobolItemDef;
	}

	/**
	 * @param cobolItemDef the cobolItemDef to set
	 */
	public void setCobolItemDef(CobolItemDef cobolItemDef) {
		this.cobolItemDef = cobolItemDef;
	}
}
