package net.sf.JRecord.IO.builders.recordDeciders;

import net.sf.JRecord.def.recordSelection.IRecordSelectionDetails;

public class RecordTypeAndRecord implements Comparable<RecordTypeAndRecord>, IRecordSelectionDetails {
	String recordTypeValue, recordName;
	int idx;
	
	public RecordTypeAndRecord(String recordTypeValue, String recordName) {
		super();
		this.recordTypeValue = recordTypeValue;
		this.recordName = recordName;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(RecordTypeAndRecord o) {
		return recordTypeValue.compareTo(o.recordTypeValue);
	}

	/**
	 * @return the recordTypeValue
	 */
	@Override
	public String getRecordTypeValue() {
		return recordTypeValue;
	}

	/**
	 * @return the recordName
	 */
	@Override
	public String getRecordName() {
		return recordName;
	}

}
