package net.sf.JRecord.External.base;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public interface IChildRecord<ExternalRecord extends BaseExternalRecord<ExternalRecord>> {

	/**
	 * @return The <b>Record-Selection</b>. The Record-Selection consists of various
	 * field tests and is used to decide which Record-Definition to use for a Line in a file
	 */
	ExternalSelection getRecordSelection();
	
	/**
	 * Get the child definition 
	 * @return child-record-definition
	 */
	ExternalRecord getExternalRecord();
}
