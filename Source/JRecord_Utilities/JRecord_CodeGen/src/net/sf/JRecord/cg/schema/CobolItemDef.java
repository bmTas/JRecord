package net.sf.JRecord.cg.schema;

import java.util.List;

import net.sf.JRecord.detailsBasic.IItemDetails;

public class CobolItemDef extends JavaDetails {
	

	private final int level;
	private final List<CobolItemDef> childItems;
	private final IItemDetails cobolItem;
	private final FieldDef fieldDefinition;
	private final ArrayDetails arraydetails;

	public CobolItemDef(
			IItemDetails item, String copybookName, String classname, 
			int level, List<CobolItemDef> children,
			FieldDef fieldDef,
			ArrayDetails arraydetails) {
		super(item.getFieldName(), copybookName, classname);
		
		this.level = level;
		this.cobolItem = item;
		this.childItems = children;
		this.fieldDefinition = fieldDef;
		this.arraydetails = arraydetails;
	}

	/**
	 * @return the level
	 */
	public int getLevel() {
		return level;
	}

	/**
	 * @return the cobolItem
	 */
	public IItemDetails getCobolItem() {
		return cobolItem;
	}

	/**
	 * @return the childItems
	 */
	public List<CobolItemDef> getChildItems() {
		return childItems;
	}

	/**
	 * @return the fieldDefinition
	 */
	public FieldDef getFieldDefinition() {
		return fieldDefinition;
	}

	/**
	 * @return the arraydetails
	 */
	public ArrayDetails getArraydetails() {
		return arraydetails;
	}

}
