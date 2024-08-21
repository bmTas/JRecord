package net.sf.JRecord.External.base;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public interface IChildRecord<ExternalRecord extends BaseExternalRecord<ExternalRecord>> {

	ExternalSelection getRecordSelection();
	ExternalRecord getExternalRecord();
}
