package net.sf.JRecord.ExternalRecordSelection;

import net.sf.JRecord.Common.Constants;

public class ExternalFieldSelection implements ExternalSelection {

	public static final String EQUALS_OPERATOR = "=";
	
	private String fieldName, fieldValue, operator=EQUALS_OPERATOR, booleanOp="";
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

	public String getFieldValue() {
		if (isCaseSensitive() || fieldValue == null) {
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
	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}


}
