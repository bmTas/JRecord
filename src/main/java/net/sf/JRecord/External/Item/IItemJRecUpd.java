package net.sf.JRecord.External.Item;

import java.util.List;

import net.sf.cb2xml.def.IItemJrUpd;

public interface IItemJRecUpd extends IItemJRec, IItemJrUpd {
	public abstract void setFormat(int formatId, String parameter);

	public abstract void setJavaType(String type);
	public abstract String getJavaType();
	
	@Override
	public abstract List<? extends IItemJRecUpd> getChildItems();

}
