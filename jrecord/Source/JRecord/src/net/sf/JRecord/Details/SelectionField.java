package net.sf.JRecord.Details;

import net.sf.JRecord.Common.FieldDetail;

public class SelectionField {
	public final String fieldName;
	public final String value;
	public final FieldDetail field;
	
	
	public SelectionField(String fieldName, FieldDetail field, String value) {
		super();
		this.fieldName = fieldName;
		this.value = value;
		this.field = field;
	}
}
