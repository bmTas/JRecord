package net.sf.JRecord.detailsSelection;

import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;

public class AndSelection extends AbsGroup {

	public AndSelection() {
		this(10);
	}
	
	public AndSelection(int size) {
		super(size);
	}


//	public AndSelection(@SuppressWarnings("rawtypes") ExternalGroupSelection sel) {
//		super(sel.size());
//	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsSelection.RecordSel#isSelected(java.util.List)
	 */
	@Override
	public boolean isSelected(List<? extends AbstractIndexedLine> line) {
		if (size() > 0) {
			RecordSel sel;
			for (int i = 0; i < size(); i++) {
				sel = get(i);

				if (! sel.isSelected(line)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
	 */
	@Override
	public boolean isSelected(AbstractIndexedLine line) {

		if (size() > 0) {
			RecordSel sel;
			for (int i = 0; i < size(); i++) {
				sel = get(i);

				if (! sel.isSelected(line)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsSelection.RecordSel#isIncluded(net.sf.JRecord.Common.AbstractIndexedLine)
	 */
	@Override
	public boolean isIncluded(AbstractIndexedLine line) {
		return true;
	}
}
