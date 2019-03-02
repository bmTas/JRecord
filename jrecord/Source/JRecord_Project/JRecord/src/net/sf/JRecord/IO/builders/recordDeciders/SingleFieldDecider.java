package net.sf.JRecord.IO.builders.recordDeciders;

import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.IRecordDeciderX;
import net.sf.JRecord.Details.LayoutDetail;

public abstract class SingleFieldDecider implements IRecordDeciderX, Cloneable {

	private final String recordTypeFieldName, defaultRecordName;

	int defaultIdx = Constants.NULL_INTEGER;

	RecordTypeAndRecord[] selections;
	
	final boolean allowOtherRecordTypes;
	private boolean layoutDefined = false;
	IFieldDetail recordTypeField;
	
	
	
	public SingleFieldDecider(
			String recordTypeFieldName, String defaultRecordName, 
			boolean allowOtherRecordTypes,
			List<RecordTypeAndRecord> selections) {
		super();
		
		this.recordTypeFieldName = recordTypeFieldName;
		this.defaultRecordName = defaultRecordName;
		this.allowOtherRecordTypes = allowOtherRecordTypes;
		this.selections = selections.toArray(new RecordTypeAndRecord[selections.size()]);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public SingleFieldDecider clone() throws CloneNotSupportedException {
		return (SingleFieldDecider) super.clone();
	}

	@Override
	public void setLayout(LayoutDetail layout) {
		if (layoutDefined) {
			throw new RecordException("This Record decider has already been defined to a layout");
		}
		layoutDefined = true;
		recordTypeField = layout.getFieldFromName(recordTypeFieldName);
		if (recordTypeField == null) {
			throw new RecordException("RecordDecider: Invalid Selection Field name: " + recordTypeFieldName);
		}
		if (defaultRecordName != null && defaultRecordName.length() > 0) {
			defaultIdx = getRecordIdx(layout, defaultRecordName);
		}

		for (int i = 0; i < selections.length; i++) {
			selections[i].idx = getRecordIdx(layout, selections[i].recordName);
		}
	}
	
	private int getRecordIdx(LayoutDetail layout, String name) {
		int idx = layout.getRecordIndex(name);
		if (idx < 0) {
			throw new RecordException("RecordDecider: Invalid default Record name: " + name);
		}
		return idx;
	}
	
	int checkDefaultIdx(String v, AbstractLine line) {
		if (defaultIdx >= 0 || allowOtherRecordTypes) {
			return defaultIdx;
		}
		throw new RecordException("RecordDecider: invalid record Decider field: " + v + " " + line.getFullLine());

	}

	public static class SmallDecider extends SingleFieldDecider {

		public SmallDecider(
				String recordTypeFieldName, String defaultRecordName, 
				boolean allowOtherRecordTypes,
				List<RecordTypeAndRecord> selections) {
			super(recordTypeFieldName, defaultRecordName, allowOtherRecordTypes, selections);
		}

		@Override
		public int getPreferedIndex(AbstractLine line) {
			Object o = line.getField(recordTypeField);
			String v = o == null ? "" : o.toString();
			for (int i = 0; i < selections.length; i++) {
				if (v.equalsIgnoreCase(selections[i].recordTypeValue)) {
					return selections[i].idx;
				}
			}
			
			return checkDefaultIdx(v, line);
		}
	}
	

	public static class SmallDeciderCaseSensitive extends SingleFieldDecider {

		public SmallDeciderCaseSensitive(
				String recordTypeFieldName, String defaultRecordName, 
				boolean allowOtherRecordTypes,
				List<RecordTypeAndRecord> selections) {
			super(recordTypeFieldName, defaultRecordName, allowOtherRecordTypes, selections);
		}

		@Override
		public int getPreferedIndex(AbstractLine line) {
			Object o = line.getField(recordTypeField);
			String v = o == null ? "" : o.toString();
			for (int i = 0; i < selections.length; i++) {
				if (v.equals(selections[i].recordTypeValue)) {
					return selections[i].idx;
				}
			}
			return checkDefaultIdx(v, line);
		}
	}
	
	public static class LargeDecider extends SingleFieldDecider {

		boolean caseInSensitive;
		String[] values;
		public LargeDecider(
				String recordTypeFieldName, String defaultRecordName, 
				boolean caseSensitive, boolean allowOtherRecordTypes,
				List<RecordTypeAndRecord> selections) {
			super(recordTypeFieldName, defaultRecordName, allowOtherRecordTypes, selections);
			
			caseInSensitive = ! caseSensitive;
			if (caseInSensitive) {
				for (int i = 0; i < super.selections.length; i++) {
					super.selections[i].recordTypeValue = super.selections[i].recordTypeValue.toUpperCase();
				}
			}
			Arrays.sort(super.selections);
			
			values = new String[super.selections.length];
			for (int i = 0; i < super.selections.length; i++) {
				values[i]= super.selections[i].recordTypeValue;
			}

		}

		@Override
		public int getPreferedIndex(AbstractLine line) {
			Object o = line.getField(recordTypeField);
			String v = o == null ? "" : o.toString();
			if (caseInSensitive) {
				v = v.toUpperCase();
			}
			int idx = Arrays.binarySearch(values, v);
			if (idx >= 0) {
				return selections[idx].idx;
			}
			return checkDefaultIdx(v, line);

		}
	}
}
