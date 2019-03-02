package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

public class CblItem {
	public static final int GROUP = 20;
	public static final int INT_FIELD = 2;
//	public static final int SIZE_FIELD = 3;
	public static final int INT_ARRAY_FIELD = 8;
	public static final int INT_OD_ARRAY_FIELD = 9;
	public static final int ARRAY = 21;
	public static final int OD_ARRAY = 22;
	
	public final int itemType, length, arraySize;
	public final String name, dependinOn;
	public final CblItem[] children;
	public final boolean hasChildren;
	
	private int count = Integer.MIN_VALUE;
	
	public static CblItem groupField(String name, CblItem... children) {
		return new CblItem(name, GROUP, 0, "", 0, children);
	}
	
	public static CblItem intField(String name) {
		return new CblItem(name, INT_FIELD, 6, "", 0, null);
	}

	public static CblItem intOdArrayField(String name, String dependinOn, int arraySize) {
		return new CblItem(name, INT_OD_ARRAY_FIELD, 0, dependinOn, arraySize, null);
	}

	public static CblItem intArrayField(String name,  int arraySize) {
		return new CblItem(name, INT_ARRAY_FIELD, 0, "", arraySize, null);
	}

//	public static CblItem sizeField(String name, int arraySize) {
//		return new CblItem(name, SIZE_FIELD, 6, "", arraySize, null);
//	}
//	
	public static CblItem odArray(String name, String dependinOn, int arraySize, CblItem... children) {
		return new CblItem(name, OD_ARRAY, 0, dependinOn, arraySize, children);
	}
	
	
	public static CblItem array(String name, int arraySize, CblItem... children) {
		return new CblItem(name, ARRAY, 0, null, arraySize, children);
	}

	protected CblItem(String name, int itemType, int length, String dependinOn, int arraySize, 
			CblItem[] children) {
		super();
		this.itemType = itemType;
		this.length = length;
		this.arraySize = arraySize;
		this.name = name;
		this.dependinOn = dependinOn;
		this.children = children;
		boolean hc = false;
		
		if (children != null) {
			for (CblItem c : children) {
				if (c.arraySize > 0) {
					hc = true;
					break;
				}
			}
		}
		hasChildren = hc;
	}
	
	
	/**
	 * @param count the count to set
	 */
	public final void setCount(int count) {
		this.count = count;
	}

	/**
	 * @param c
	 * @return
	 */
	public int getCount(ArrayIndex indexs, ArrayIndex countIndexs) {
		if (count >= 0) {
			return count;
		}
		if (this.itemType == ARRAY || this.itemType == INT_ARRAY_FIELD) {
			return this.arraySize;
		}
		return (indexs.getValue(1)+ countIndexs.getIndex(indexs.getNumberOfIndexs()))
			 % (this.arraySize + 1);
	}


}
