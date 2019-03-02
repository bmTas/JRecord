/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl.cobol;

import net.sf.JRecord.Details.LayoutDetail;


public class HierarchyRecordDef {

	public static HierarchyRecordDef getMrhfRecordDef(LayoutDetail schema) {
		return new HierarchyRecordDef(schema, "12345") .setChildren(
				new HierarchyRecordDef("Rec-1", 1,3,5) .setChildren(
					new HierarchyRecordDef("Rec-11", 0, 1,3,5) .setChildren(
							new HierarchyRecordDef("Rec-111", 0, 1,3,5),
							new HierarchyRecordDef("Rec-112", 0, 1,3,5),
							new HierarchyRecordDef("Rec-113", 0, 1,3,5)
					),
					new HierarchyRecordDef("Rec-12", 0, 1, 3) .setChildren(
							new HierarchyRecordDef("Rec-121", 0, 1, 3)
					)
				),
				new HierarchyRecordDef("Rec-2", 1,3,5) .setChildren(
						new HierarchyRecordDef("Rec-21", 0, 1,3,5) .setChildren(
								new HierarchyRecordDef("Rec-211", 0, 1,3,5)
						)
					)
		);

	}
	
	private LayoutDetail schema;
	public  final String name;
	private int recIdx = -1;
	public final int[] counts;
	public HierarchyRecordDef[] children;
	int countIdx = 1;
	public final int recId;
	

	protected HierarchyRecordDef(LayoutDetail schema, String name, int...counts) {
		this(name, counts);
		this.schema = schema;
	}
	
	protected HierarchyRecordDef(String name, int...counts) {
		super();
		this.name = name;
		this.counts = counts;
		this.recId = Integer.parseInt(name.substring(4));
	}


	/**
	 * @param children the children to set
	 */
	public HierarchyRecordDef setChildren(HierarchyRecordDef... children) {
		this.children = children;
		updateSchema();
		return this;
	}

	public void updateSchema() {
		if (schema != null && children != null) {
			for (HierarchyRecordDef c : children) {
				c.schema = schema;
				c.updateSchema();
			}
		}

	}
	/**
	 * @return the recIdx
	 */
	public final int getRecIdx() {
		if (recIdx < 0) {
			recIdx = schema.getRecordIndex(name);
		}
		return recIdx;
	}

	
	final int nextCount() {
		if (countIdx >= counts.length) {
			countIdx = 0;
		}
		return counts[countIdx++];
	}

}
