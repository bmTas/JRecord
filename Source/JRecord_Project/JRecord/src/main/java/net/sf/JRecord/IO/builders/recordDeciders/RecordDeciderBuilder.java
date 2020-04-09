package net.sf.JRecord.IO.builders.recordDeciders;

import java.util.ArrayList;

import net.sf.JRecord.def.IO.builders.recordDeciders.IRecordDeciderBuilder;
import net.sf.JRecord.def.IO.builders.recordDeciders.ISingleFieldDeciderBuilder;

public class RecordDeciderBuilder implements IRecordDeciderBuilder {

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.recordDeciders.IRecordDeciderBuilder#singleFieldDeciderBuilder(java.lang.String, boolean)
	 */
	@Override
	public ISingleFieldDeciderBuilder singleFieldDeciderBuilder(String fieldName, boolean allowOtherKeyValues) {
		return new SingleFieldDeciderBldr(fieldName, null, allowOtherKeyValues);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.recordDeciders.IRecordDeciderBuilder#singleFieldDeciderBuilder(java.lang.String, java.lang.String)
	 */
	@Override
	public ISingleFieldDeciderBuilder singleFieldDeciderBuilder(String fieldName, String defaultRecordName) {
		return new SingleFieldDeciderBldr(fieldName, defaultRecordName, false);
	}

	private static class SingleFieldDeciderBldr implements ISingleFieldDeciderBuilder {

		final String selectionFieldName, defaultRecordName;
		boolean caseSensitive; 
		final boolean allowOtherKeyValues;
		ArrayList<RecordTypeAndRecord> records = new ArrayList<RecordTypeAndRecord>();
		
		SingleFieldDeciderBldr(
				String selectionFieldName, String defaultRecordName,
				boolean allowOtherKeyValues) {
			
			this.selectionFieldName = selectionFieldName;
			this.defaultRecordName = defaultRecordName;
			this.allowOtherKeyValues = allowOtherKeyValues;
		}
		
		
		/**
		 * @param caseSensitive the caseSensitive to set
		 */
		@Override
		public SingleFieldDeciderBldr setCaseSensitive(boolean caseSensitive) {
			this.caseSensitive = caseSensitive;
			
			return this;
		}


		/* (non-Javadoc)
		 * @see net.sf.JRecord.def.IO.builders.recordDeciders.ISingleFieldDecider#addRecord(java.lang.String, java.lang.String)
		 */
		@Override
		public ISingleFieldDeciderBuilder addRecord(String selectionValue, String recordName) {

			records.add(new RecordTypeAndRecord(selectionValue, recordName));
			
			return this;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.def.IO.builders.recordDeciders.ISingleFieldDecider#build()
		 */
		@Override
		public SingleFieldDecider build() {
			SingleFieldDecider ret;
			if (records.size() < 10) {
				if (caseSensitive) {
					ret = new SingleFieldDecider.SmallDeciderCaseSensitive(
							selectionFieldName, defaultRecordName, allowOtherKeyValues, records);
				} else {
					ret = new SingleFieldDecider.SmallDecider(
							selectionFieldName, defaultRecordName, allowOtherKeyValues, records);
				}
			} else {
				ret = new SingleFieldDecider.LargeDecider(
						selectionFieldName, defaultRecordName, caseSensitive, 
						allowOtherKeyValues, records);
			}
			return ret;
		}
	}
}
