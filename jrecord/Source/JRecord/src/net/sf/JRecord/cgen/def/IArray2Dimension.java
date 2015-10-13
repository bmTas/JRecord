package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Common.IFieldDetail;

public interface IArray2Dimension {

	public abstract IFieldDetail get(int index1, int index2);
	public abstract int getArrayLength(int indexNumber);
	
}
