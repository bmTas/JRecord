package net.sf.JRecord.def.IO.builders.recordDeciders;

import net.sf.JRecord.Details.IRecordDeciderX;

public interface ISingleFieldDeciderBuilder {

	/**
	 * Wether tests should be case sensitive or not. Default is no
	 * 
	 * @param caseSensistive tests are case sensitive
	 * 
	 * @return this Decider builder so more records can be defines
	 */
	ISingleFieldDeciderBuilder setCaseSensitive(boolean caseSensitive);
	
	/**
	 * Add a Record-Type-Value and the 
	 * 
	 * @param recordTypeValue value top test against in the Data-Line 
	 * @param recordName Name of the Corresponding Record.
	 * 
	 * @return this Decider builder so more records can be defines
	 */
	ISingleFieldDeciderBuilder addRecord(String recordTypeValue, String recordName);
	
	/**
	 * Create the RecordDecider
	 * @return newly created <b>record-decider</b>
	 */
	IRecordDeciderX build();
}
