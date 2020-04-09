/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.detailsSelection;

import net.sf.JRecord.Common.IGetFieldByName;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public class Convert {
//	private int lvl = 0;
	public RecordSel convert(ExternalSelection sel, IGetFieldByName recDef) {
//		lvl = 0;
		return convertI(sel, recDef);
	}
	
	
	@SuppressWarnings("unchecked")
	private RecordSel convertI(ExternalSelection sel, /*AbstractRecordX<? extends IFieldDetail>*/ IGetFieldByName recDef) {
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
			AndSelection and = new AndSelection(g.size());
			copy(g, and, recDef);
			ret = and;
			break;
		case ExternalSelection.TYPE_OR:
			g = (ExternalGroupSelection<ExternalSelection>) sel;
//			System.out.println(" Or");
			OrSelection or = new OrSelection(g.size());
			ret = copy(g, or, recDef);
			break;
		}
//		lvl -= 1;

		return ret;
	}

	private RecordSel copy(ExternalGroupSelection<ExternalSelection> g, ExternalGroupSelection<RecordSel> to, IGetFieldByName r) {
		for (int i = 0; i < g.size(); i++) {
			to.add(convertI(g.get(i), r));
		}

		if (to.size() == 1) {
			return to.get(0);
		}
		return (RecordSel) to;
	}
}
