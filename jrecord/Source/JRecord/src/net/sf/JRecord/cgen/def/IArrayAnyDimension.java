package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Common.IFieldDetail;

public interface IArrayAnyDimension {

	public abstract IFieldDetail getField(int...indexs);
}
