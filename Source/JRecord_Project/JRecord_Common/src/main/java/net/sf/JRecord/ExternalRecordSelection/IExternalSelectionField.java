package net.sf.JRecord.ExternalRecordSelection;

public interface IExternalSelectionField extends ExternalSelection {

	public String getFieldName();
	public String getOperator();
	public String getFieldValue();
	
}
