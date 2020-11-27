package net.sf.JRecord.External.Item;

import java.util.List;

import net.sf.cb2xml.def.IItemJr;

public interface IItemJRec extends IItemJr {
	public abstract int getFormatId();
	public abstract String getParameter();

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItem#getChildItems()
	 */
	@Override
	public abstract List<? extends IItemJRec> getChildItems();

}
