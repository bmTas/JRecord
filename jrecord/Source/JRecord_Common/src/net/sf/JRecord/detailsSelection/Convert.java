package net.sf.JRecord.detailsSelection;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public class Convert {
//	private int lvl = 0;
	public RecordSel convert(ExternalSelection sel, AbstractRecordX<? extends IFieldDetail> recDef) {
//		lvl = 0;
		return convertI(sel, recDef);
	}
	private RecordSel convertI(ExternalSelection sel, AbstractRecordX<? extends IFieldDetail> recDef) {
		RecordSel ret=null;
		ExternalGroupSelection<ExternalSelection> g;

//		for (int i = 0; i < lvl; i++) System.out.print(" ");
//		lvl += 1;

		switch (sel.getType()) {
		case ExternalSelection.TYPE_ATOM:
			ExternalFieldSelection f = (ExternalFieldSelection) sel;
//			System.out.println(" Field " + f.getFieldName()
//					+" " + f.getOperator() + " " + f.getFieldValue());

			ret = FieldSelectX.get(f, recDef.getField(f.getFieldName()));
			break;
		case ExternalSelection.TYPE_AND:
			g = (ExternalGroupSelection<ExternalSelection>) sel;
//			System.out.println(" And");
			AndSelection and = new AndSelection(g);
			copy(g, and, recDef);
			ret = and;
			break;
		case ExternalSelection.TYPE_OR:
			g = (ExternalGroupSelection<ExternalSelection>) sel;
//			System.out.println(" Or");
			OrSelection or = new OrSelection(g);
			ret = copy(g, or, recDef);
			break;
		}
//		lvl -= 1;

		return ret;
	}

	private RecordSel copy(ExternalGroupSelection<ExternalSelection> g, ExternalGroupSelection<RecordSel> to, AbstractRecordX<? extends IFieldDetail> r) {
		for (int i = 0; i < g.size(); i++) {
			to.add(convertI(g.get(i), r));
		}

		if (to.size() == 1) {
			return to.get(0);
		}
		return (RecordSel) to;
	}
}
