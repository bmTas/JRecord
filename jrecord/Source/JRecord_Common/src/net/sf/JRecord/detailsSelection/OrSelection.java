package net.sf.JRecord.detailsSelection;

import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;

public class OrSelection extends AbsGroup {

	public OrSelection() {
		this(10);
	}

	public OrSelection(int size) {
		super (size);
		setType(TYPE_OR);
	}
//	public OrSelection(@SuppressWarnings("rawtypes") ExternalGroupSelection sel) {
//		super(sel.size());
//		setType(TYPE_OR);
//	}


	/**
	 * @see net.sf.JRecord.detailsSelection.RecordSel#isSelected(java.util.List)
	 */
	@Override
	public boolean isSelected(List<? extends AbstractIndexedLine> lines) {
		if (size() > 0) {
			RecordSel sel;

			for (int i = 0; i < size(); i++) {
				sel = get(i);

				if (sel.isSelected(lines)) {
					return true;
				}
			}
		} else {
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

			//System.out.println();
			//System.out.print("Or --> ");
			for (int i = 0; i < size(); i++) {
				sel = get(i);

				if (sel.isSelected(line)) {
					//System.out.println();
					return true;
				}
			}
			//System.out.println();
		} else {
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
