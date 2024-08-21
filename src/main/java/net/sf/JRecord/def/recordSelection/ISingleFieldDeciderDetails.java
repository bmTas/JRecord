package net.sf.JRecord.def.recordSelection;

import java.util.List;

public interface ISingleFieldDeciderDetails {

	/**
	 * @return the recordTypeFieldName
	 */
	String getRecordTypeFieldName();

	/**
	 * @return the defaultRecordName
	 */
	String getDefaultRecordName();

	/**
	 * @return the allowOtherRecordTypes
	 */
	boolean isAllowOtherRecordTypes();

	List<IRecordSelectionDetails> getRecordSelectionDetails();

}