package net.sf.JRecord.ExternalRecordSelection;


public class StreamLine<Selection extends ExternalSelection> {
	
	private static final StreamLine<ExternalSelection> externalStreamLine
				= new StreamLine<ExternalSelection>();
	
	public final Selection streamLine(Selection sel) {
		if (sel instanceof ExternalGroupSelection) {
			ExternalGroupSelection<Selection> grp = (ExternalGroupSelection<Selection>) sel;
			if (grp.size() == 1) {
				return streamLine(grp.get(0));
			} else {
				for (int i = 0; i < grp.size(); i++) {
					grp.set(i, streamLine(grp.get(i)));
				}
			}
		}
		return sel;
	}

	/**
	 * @return the externalStreamLine
	 */
	public static StreamLine<ExternalSelection> getExternalStreamLine() {
		return externalStreamLine;
	}
}
