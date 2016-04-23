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
      
package net.sf.JRecord.ExternalRecordSelection;

import net.sf.JRecord.Common.Constants;

public class ExternalFieldSelection implements ExternalSelection {

	public static final String EQUALS_OPERATOR = "=";
	
	private String fieldName, fieldValue, booleanOp="",
			operator=EQUALS_OPERATOR;
	private boolean caseSensitive = true;
	private static final String[] VALID_OPS = Constants.VALID_COMPARISON_OPERATORS;

	public ExternalFieldSelection() {
		super();
	}


	public ExternalFieldSelection(String name, String value) {
		fieldName = name;
		fieldValue = value;
	}

	public ExternalFieldSelection(String name, String value, String op) {
		fieldName = name;
		fieldValue = value;
		if (op != null) {
			for (int i = 0; i < VALID_OPS.length; i++) {
				if (VALID_OPS[i].equalsIgnoreCase(op)) {
					operator = op;
					break;
				}
			}
		}
	}


	public void set(ExternalFieldSelection fs) {
		fieldName = fs.fieldName;
		fieldValue = fs.fieldValue;
		booleanOp = fs.booleanOp;
		operator = fs.operator;
		caseSensitive = fs.caseSensitive;
	}



	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}
	
	public String getRawFieldValue() {
		return fieldValue;
	}

	public String getFieldValue() {
		if (isCaseSensitive() /*|| EQUALS_OPERATOR.equals(getOperator()) */ || fieldValue == null) {
			return fieldValue;
		}
		return fieldValue.toLowerCase();
	}

	public void setFieldValue(String fieldValue) {
		this.fieldValue = fieldValue;
	}

	public String getOperator() {
		return operator;
	}

	public void setOperator(String operator) {

		this.operator = null;
		if (operator != null) {
			this.operator = operator.trim();
		}
	}

	public String getBooleanOp() {
		return booleanOp;
	}

	public void setBooleanOp(String booleanOp) {
		this.booleanOp = booleanOp;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.RecordSelection.ExternalSelection#getType()
	 */
	@Override
	public int getType() {
		return ExternalSelection.TYPE_ATOM;
	}


	@Override
	public int getSize() {
		return 1;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getElementCount()
	 */
	@Override
	public int getElementCount() {
		return 1;
	}


	/**
	 * @return the caseSensitive
	 */
	public boolean isCaseSensitive() {
		return caseSensitive;
	}


	/**
	 * @param caseSensitive the caseSensitive to set
	 */
	public ExternalFieldSelection setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
		return this;
	}


}
