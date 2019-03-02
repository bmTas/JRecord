package net.sf.JRecord.IO.builders.recordDeciders;

public class RecordTypeAndRecord implements Comparable<RecordTypeAndRecord>{
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

}
