package net.sf.JRecord.Option;

import java.util.TreeMap;

public class Options {

	public static final IRecordPositionOption RP_FIRST_RECORD_IN_FILE = new OptionType("First");
	public static final IRecordPositionOption RP_MIDDLE_RECORDS       = new OptionType("Middle");
	public static final IRecordPositionOption RP_LAST_RECORD_IN_FILE  = new OptionType("Last");
	
	public static final OptionMap<IRecordPositionOption> recordPositionMap = new OptionMap<IRecordPositionOption>(
			RP_FIRST_RECORD_IN_FILE, RP_MIDDLE_RECORDS, RP_LAST_RECORD_IN_FILE
			);
	
	
	public static class  OptionMap<OT extends IOptionType> {
		final TreeMap<String, OT> map = new TreeMap<String, OT>();
		
		@SafeVarargs
		private OptionMap(OT...iOptionTypes) {
			for (OT t : iOptionTypes) {
				map.put(t.toString().toLowerCase(), t);
			}
		}
		
		public IOptionType get(String name) {
			if (name == null) {
				return null;
			}
			
			return map.get(name.toLowerCase());
		}
	}
}
