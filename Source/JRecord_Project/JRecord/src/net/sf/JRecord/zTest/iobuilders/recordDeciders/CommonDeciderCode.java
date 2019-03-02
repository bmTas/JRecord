package net.sf.JRecord.zTest.iobuilders.recordDeciders;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.IRecordDeciderX;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.builders.recordDeciders.RecordTypeAndRecord;
import net.sf.JRecord.IO.builders.recordDeciders.SingleFieldDecider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.recordDeciders.ISingleFieldDeciderBuilder;

public class CommonDeciderCode {
	public static final String RECORD_TYPE = "Record-Type";

	static enum DeciderType {
		 SMALL_DECIDER,
		 SMALL_CASE_SENSITIVE_DECIDER,
		 LARGE_DECIDER,
		 LARGE_CASE_SENSITIVE_DECIDER
	}

	String Copybook 
		= "       01  decider-tst.\n"
		+ "           03  Record-Type                     pic xx.\n"
		+ "           03  Record-A                        pic x(10).\n"
		+ "           03  Record-B   redefines Record-A   pic x(10).\n"
		+ "           03  Record-C   redefines Record-A   pic x(10).\n"
		+ "           03  Record-D   redefines Record-A   pic x(10).\n"
		+ "           03  Record-E   redefines Record-A   pic x(10).\n"
		+ "           03  Record-F   redefines Record-A   pic x(10).\n"
		+ "           03  Record-G   redefines Record-A   pic x(10).\n"
		+ "           03  Record-H   redefines Record-A   pic x(10).\n"
		+ "           03  Record-I   redefines Record-A   pic x(10).\n"
		+ "           03  Record-J   redefines Record-A   pic x(10).\n";
	
	SelValue[] values = {
			new SelValue("A", "Record-A"),
			new SelValue("B", "Record-B"),
			new SelValue("C", "Record-C"),
			new SelValue("D", "Record-D"),
			new SelValue("E", "Record-E"),
			new SelValue("F", "Record-F"),
			new SelValue("G", "Record-G"),
			new SelValue("H", "Record-H"),
			new SelValue("I", "Record-I"),
			new SelValue("J", "Record-J"),
			new SelValue("aA", "Record-A"),
			new SelValue("bB", "Record-B"),
			new SelValue("Cc", "Record-C"),
			new SelValue("DD", "Record-D"),
			new SelValue("Ee", "Record-E"),
	};
	
	public ICobolIOBuilder bldr() {
		return JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(Copybook), "decider-tst")
					.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE);
	}
	
	public void addRecordSelections(ISingleFieldDeciderBuilder singleFieldDecider, int count) {
	
		count = Math.min(count, values.length);
		for (int i = 0; i < count; i++) {
			singleFieldDecider.addRecord(values[i].val, values[i].record);
		}
	}

//	public List<SelectionValue> getSelectionValues(int count) {
//		
//		SelValue[] selValues = getSelValues(count);
//		return toSelectionValues(selValues);
//	}

	/**
	 * @param selValues
	 * @return
	 */
	public static List<RecordTypeAndRecord> toSelectionValues(SelValue[] selValues) {
		ArrayList<RecordTypeAndRecord> ret = new ArrayList<RecordTypeAndRecord>();
		for (SelValue v : selValues) {
			ret.add(new RecordTypeAndRecord(v.val, v.record));
		}
		
		return ret;
	}

	public SelValue[] getSelValues(int count) {
		
		count = Math.min(count, values.length);
		SelValue[] ret = new SelValue[count];
		System.arraycopy(values, 0, ret, 0, count);
		
		return ret;
	}

	
	public static class SelValue {
		final String val, record;

		public SelValue(String val, String record) {
			super();
			this.val = val;
			this.record = record;
		}
	}
	public static interface ISfDeciderBldr {

		public DeciderType getType();
		IRecordDeciderX build(String defaultRecordName, boolean allowOtherKeys, SelValue[] selections);

	}
	
	public static class SfDeciderBldr implements ISfDeciderBldr {
		DeciderType type;
		
		public SfDeciderBldr(DeciderType type) {
			super();
			this.type = type;
		}
		
		/* (non-Javadoc)
		 * @see net.sf.JRecord.zTest.iobuilders.recordDeciders.ISfDeciderBldr#build(java.lang.String, boolean, java.util.List)
		 */
		@Override
		public SingleFieldDecider build(String defaultRecordName, boolean allowOtherKeys, SelValue[] selvalues) {
			List<RecordTypeAndRecord> selections = toSelectionValues(selvalues);
			switch (type) {
			case SMALL_DECIDER: 				return new SingleFieldDecider.SmallDecider(RECORD_TYPE, defaultRecordName, allowOtherKeys, selections);
			case SMALL_CASE_SENSITIVE_DECIDER:  return new SingleFieldDecider.SmallDeciderCaseSensitive(RECORD_TYPE, defaultRecordName, allowOtherKeys, selections);
			case LARGE_DECIDER: 				return new SingleFieldDecider.LargeDecider(RECORD_TYPE, defaultRecordName, false, allowOtherKeys, selections);
			case LARGE_CASE_SENSITIVE_DECIDER:  return new SingleFieldDecider.LargeDecider(RECORD_TYPE, defaultRecordName, true, allowOtherKeys, selections);
			default:
				throw new RuntimeException();
			}
			
		}

		/**
		 * @return the type
		 */
		public DeciderType getType() {
			return type;
		}
	}
	
	public static class JrDeciderBldr implements ISfDeciderBldr {
		IRecordDeciderX lastDecider;
		final boolean caseSensitive;
		
		
		public JrDeciderBldr(boolean caseSensitive) {
			super();
			this.caseSensitive = caseSensitive;
		}

		@Override
		public IRecordDeciderX build(String defaultRecordName, boolean allowOtherKeys, SelValue[] selvalues) {
			ISingleFieldDeciderBuilder deciderBuilder;
			if (defaultRecordName == null) {
				deciderBuilder
						= JRecordInterface1.RECORD_DECIDER_BUILDER
							.singleFieldDeciderBuilder(RECORD_TYPE, allowOtherKeys);
			} else {
				deciderBuilder
						= JRecordInterface1.RECORD_DECIDER_BUILDER
							.singleFieldDeciderBuilder(RECORD_TYPE, defaultRecordName);
			}
			
			deciderBuilder.setCaseSensitive(caseSensitive);
			for (SelValue v : selvalues) {
				deciderBuilder.addRecord(v.val, v.record);
			}
			
			lastDecider = deciderBuilder.build();
			return lastDecider;
		}
		
		public DeciderType getType() {
			DeciderType type = DeciderType.LARGE_DECIDER;
			if (lastDecider instanceof  SingleFieldDecider.SmallDecider) {
				type = DeciderType.SMALL_DECIDER;
			} else if (lastDecider instanceof  SingleFieldDecider.SmallDeciderCaseSensitive) {
				type = DeciderType.SMALL_CASE_SENSITIVE_DECIDER;
			} else if (caseSensitive){
				type = DeciderType.LARGE_CASE_SENSITIVE_DECIDER;
			}
			return type;
		}
	}
	
}
