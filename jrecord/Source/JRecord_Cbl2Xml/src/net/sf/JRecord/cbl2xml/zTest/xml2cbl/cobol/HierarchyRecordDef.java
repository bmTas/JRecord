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
