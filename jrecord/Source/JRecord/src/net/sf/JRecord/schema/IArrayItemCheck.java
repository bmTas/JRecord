package net.sf.JRecord.schema;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.Item;

public interface IArrayItemCheck {
	public static final int R_PROCESS = 0;
	public static final int R_SKIP    = 1;
	public static final int R_STOP    = 2;
	
	public int checkItem(AbstractLine line, Item item, int[] indexs, int index);
	
	public int getCount(AbstractLine line, Item item, int[] indexs, int defaultCount);
	
	public void updateForCount(AbstractLine line, Item item, int[] indexs, int count);
}
