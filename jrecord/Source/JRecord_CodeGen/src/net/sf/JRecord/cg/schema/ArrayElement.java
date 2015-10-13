package net.sf.JRecord.cg.schema;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public final class ArrayElement extends JavaDetails {
	final String arrayName;
	final int[] indexs;
	final boolean special, first;
	/**
	 * @return the special
	 */
	public final boolean isSpecial() {
		return special;
	}
	final int specialLevel;
	
	public static ArrayElement newArrayItem(String arrayElement) {
		arrayElement = arrayElement.trim();
		int idx = arrayElement.indexOf('(');
		return new ArrayElement(arrayElement.substring(0, idx - 1), idx, arrayElement);
	}
	private ArrayElement(String arrayName, int idx, String arrayElement) {
		super(arrayName);
		this.arrayName = arrayName;
		
		StringTokenizer t = new StringTokenizer(arrayElement.substring(idx+1, arrayElement.length() - 1), ",");
		List<Integer> li = new ArrayList<>(7);
		while (t.hasMoreTokens()) {
			li.add(Integer.parseInt(t.nextToken().trim()));
		}
		indexs = new int[li.size()];
		for (int i = 0; i < indexs.length; i++) {
			indexs[i] = li.get(i);
		}
		int oneCount = 0;
		int sLevel = indexs.length;
		
		for (int i = 0; sLevel >= 0 && i < indexs.length; i++) {
			switch (indexs[i]) {
			case 0: break;
			case 1: 
				if (oneCount > 0) {
					sLevel = -1;
					break;
				}
				sLevel = i;
				oneCount += 1;
				break;
			default:
				sLevel = -1;
			}
		}
		special = sLevel >= 0;
		specialLevel = sLevel;
		first = special && oneCount == 0;
	}
	
}
