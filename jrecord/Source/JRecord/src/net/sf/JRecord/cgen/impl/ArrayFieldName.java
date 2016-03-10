package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.cgen.def.IFieldName1Dimension;
import net.sf.JRecord.cgen.def.IFieldName2Dimension;
import net.sf.JRecord.cgen.def.IFieldName3Dimension;

public class ArrayFieldName implements IFieldName1Dimension, IFieldName2Dimension, IFieldName3Dimension {

	private final String name;
	
	
	public ArrayFieldName(String name) {
		this.name = name;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName1Dimension#get(int)
	 */
	@Override
	public String get(int index1) {
		return genName(index1);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName2Dimension#get(int, int)
	 */
	@Override
	public String get(int index1, int index2) {
		return genName(index1, index2);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName3Dimension#get(int, int, int)
	 */
	@Override
	public String get(int index1, int index2, int index3) {
		return genName(index1, index2, index3);
	}

	
	private String genName(int... indexs) {
		StringBuilder b = new StringBuilder(name);
		String sep = " (";
		
		for (int idx : indexs) {
			b.append(sep).append(idx);
			sep = ", ";
		}
		return b.append(')').toString();
	}
}
