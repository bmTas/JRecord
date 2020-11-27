package net.sf.JRecord.cg.schema;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public class FieldSelection implements ExternalSelection {

	private final String operator, value;
	private final FieldDef field;
	
	
	
	protected FieldSelection(FieldDef field, String operator, String value) {
		super();
		this.operator = operator;
		this.value = value;
		this.field = field;
	}

	@Override
	public int getType() {
		return ExternalSelection.TYPE_ATOM;
	}

	@Override
	public int getSize() {
		return 0;
	}

	@Override
	public int getElementCount() {
		return 0;
	}

	/**
	 * @return the operator 
	 */
	public final String getOperator() {
		return operator;
	}

	/**
	 * @return the value
	 */
	public final String getValue() {
		return value;
	}

	/**
	 * @return the field
	 */
	public final FieldDef getField() {
		return field;
	}

}
