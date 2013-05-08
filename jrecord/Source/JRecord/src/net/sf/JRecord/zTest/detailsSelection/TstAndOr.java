package net.sf.JRecord.zTest.detailsSelection;

import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.detailsSelection.AndSelection;
import net.sf.JRecord.detailsSelection.FieldSelect;
import net.sf.JRecord.detailsSelection.FieldSelectX;
import net.sf.JRecord.detailsSelection.OrSelection;

public class TstAndOr extends TestCase {
	private static final FieldSelect yes = FieldSelectX.getTrueSelection();
	private static final FieldSelect no = new FieldSelect("", "", "", null) {

		@Override
		public boolean isSelected(Object line) {
			return false;
		}
	};


	private static final FieldSelect[][]  items = {
		{no,  no},
		{no,  yes},
		{yes, no},
		{yes, yes},

		{no,  no , no},
		{no,  yes, no},
		{yes, no , no},
		{yes, yes, no},
		{no,  no,  yes},
		{no,  yes, yes},
		{yes, no,  yes},
		{yes, yes, yes},

		{no,  no,  no, no},
		{no,  yes, no, no},
		{yes, no,  no, no},
		{yes, yes, no, no},
		{no,  no,  yes, no},
		{no,  yes, yes, no},
		{yes, no,  yes, no},
		{yes, yes, yes, no},
		{no,  no,  no, yes},
		{no,  yes, no, yes},
		{yes, no,  no, yes},
		{yes, yes, no, yes},
		{no,  no,  yes, yes},
		{no,  yes, yes, yes},
		{yes, no,  yes, yes},
		{yes, yes, yes, yes},

	};

	private static final boolean[] andResult = {
			false, false, false, true,
			false, false, false, false, false, false, false, true,

			false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true,
	};
	private static final boolean[] orResult = {
		false, true, true, true,
		false, true, true, true, true, true, true, true,

		false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
	};


	public void testAnd() {
		AndSelection and;

		for (int i = 0; i < items.length; i++) {
			and = new AndSelection();
			for (FieldSelect f : items[i]) {
				and.add(f);
			}

			assertEquals(andResult[i], and.isSelected((AbstractIndexedLine) null));
			assertEquals(andResult[i], and.isSelected((List<AbstractIndexedLine>) null));
		}
	}


	public void testOr() {
		OrSelection and;

		for (int i = 0; i < items.length; i++) {
			and = new OrSelection();
			for (FieldSelect f : items[i]) {
				and.add(f);
			}

			assertEquals(orResult[i], and.isSelected((AbstractIndexedLine) null));
			assertEquals(orResult[i], and.isSelected((List<AbstractIndexedLine>) null));
		}
	}
}
