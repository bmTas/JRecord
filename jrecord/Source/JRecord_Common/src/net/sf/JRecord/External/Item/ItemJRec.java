/**
 * 
 */
package net.sf.JRecord.External.Item;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.IItem;

/**
 * @author bruce
 *
 */
public class ItemJRec extends Item implements IItemJRecUpd {

	public static List<ItemJRec> EMPTY_LIST = Collections.emptyList();
	private int formatId=0;
	private String parameter="", javaType; //JRecord Type Identifier

	private List<ItemJRec> childItems = EMPTY_LIST;

	
	public ItemJRec(BaseItem parentItem, IItem item) {
		super(parentItem, item.getLevelNumber(), item.getLevelString(), item.getFieldName());
		super.set(item);
		
		if (item instanceof IItemJRec) {
			IItemJRec itm = (IItemJRec) item;
			
			this.formatId = itm.getFormatId();
			this.parameter = itm.getParameter();
		}
		
		List<? extends IItem> childItms = item.getChildItems();
		for (int i = 0; i < childItms.size(); i++) {
			new ItemJRec(this, childItms.get(i));
		}
	}

	public void addItem(Item item) {
		addItem((ItemJRec) item);
	}

	public void addItem(ItemJRec item) {
		if (childItems == EMPTY_LIST) {
			childItems = new ArrayList<ItemJRec>(5);
		}
		childItems.add(item);
		add(item);
	}

	/**
	 * @return the childItems
	 */
	public final List<ItemJRec> getChildItems() {
		return childItems;
	}

	/**
	 * @return the formatId
	 */
	@Override
	public final int getFormatId() {
		return formatId;
	}

	/**
	 * @return the parameter
	 */
	@Override
	public final String getParameter() {
		return parameter;
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItemJrUpd#setFormat(int, java.lang.String)
	 */
	@Override
	public void setFormat(int formatId, String parameter) {
		this.formatId = formatId;
		this.parameter = parameter;
	}

	/**
	 * @return the generateType
	 */
	@Override
	public final String getJavaType() {
		return javaType;
	}

	/**
	 * @param generateType the generateType to set
	 */
	@Override
	public final void setJavaType(String generateType) {
		this.javaType = generateType;
	}

}
