/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *
 *    Sub-Project purpose: Provide support for reading Cobol-Data files
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *
 *                 Author: Bruce Martin
 *
 *                License: LGPL 2.1 or latter
 *
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.External.Item;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.IItem;

/**
 * JRecords enhanced version of cb2xml's item class. This class
 * Takes a cb2xml-Item as input and creates JRecord equivalent
 * and copies all child items
 * 
 * @author Bruce Martin
 *
 */
public class ItemJRec extends Item implements IItemJRecUpd {

	public static List<ItemJRec> EMPTY_LIST = Collections.emptyList();
	private int formatId = 0;
	private String parameter = "", 
			javaType; //JRecord Type Identifier

	private final List<ItemJRec> childItems;// = EMPTY_LIST;

	
	public ItemJRec(BaseItem parentItem, IItem item) {
		super(parentItem, item.getLevelNumber(), item.getLevelString(), item.getFieldName());
		super.set(item);
		
		if (item instanceof IItemJRec) {
			IItemJRec itm = (IItemJRec) item;
			
			this.formatId = itm.getFormatId();
			this.parameter = itm.getParameter();
		}
		
		List<? extends IItem> sourceChildItems = item.getChildItems();
		int numberOfChildItems = sourceChildItems.size();
		this.childItems = numberOfChildItems == 0 ? EMPTY_LIST : new ArrayList<>(numberOfChildItems);
		for (IItem childItm : sourceChildItems) {
            new ItemJRec(this, childItm);
        }
		
		List<? extends ICondition> conditions = item.getConditions();
		for (ICondition c : conditions) {
			if (c instanceof Condition) {
				super.addCondition((Condition) c);
			}
		}
	}

	public void addItem(Item item) {
		addItem((ItemJRec) item);
	}

	public void addItem(ItemJRec item) {
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
	 * @return the equivalent Java-Type for this Cobol-field. 
	 */
	@Override
	public final String getJavaType() {
		return javaType;
	}

	/**
	 * @param generateType the Java type to set. 
	 */
	@Override
	public final void setJavaType(String generateType) {
		this.javaType = generateType;
	}

}
