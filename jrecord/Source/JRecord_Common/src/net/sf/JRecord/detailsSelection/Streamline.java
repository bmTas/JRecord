package net.sf.JRecord.detailsSelection;

import net.sf.JRecord.ExternalRecordSelection.StreamLine;

public class Streamline extends StreamLine<RecordSel> {
	
	private static final Streamline instance = new Streamline();

	/**
	 * @return the instance
	 */
	public static Streamline getInstance() {
		return instance;
	}
	
}
