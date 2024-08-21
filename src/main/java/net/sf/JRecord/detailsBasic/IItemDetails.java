package net.sf.JRecord.detailsBasic;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.External.Item.IItemJRec;
import net.sf.JRecord.cgen.def.IArrayExtended;
import net.sf.JRecord.detailsBasic.ItemDtl.ItemType;

/**
 * Extended Cobol-Item Definition
 * @author Bruce Martin
 *
 */
public interface IItemDetails extends IItemJRec {

	List<? extends IItemDetails> getChildItems();

	/**
	 * @return the fieldDefinition
	 */
	IFieldDetail getFieldDefinition();

	/**
	 * @return the arrayDefinition
	 */
	IArrayExtended getArrayDefinition();

	/**
	 * @return the itemType
	 */
	ItemType getItemType();

	boolean isLeaf();
	
	int getLevelIndex();

	String getJavaType();
}