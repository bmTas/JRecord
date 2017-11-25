package net.sf.JRecord.detailsBasic;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;
import net.sf.JRecord.detailsBasic.ItemDtl.ItemType;
import net.sf.cb2xml.def.IItemJr;

/**
 * Extended Cobol-Item Definition
 * @author bruce
 *
 */
public interface IItemDetails extends IItemJr {

	List<? extends IItemDetails> getChildItems();

	/**
	 * @return the fieldDefinition
	 */
	IFieldDetail getFieldDefinition();

	/**
	 * @return the arrayDefinition
	 */
	IArrayAnyDimension getArrayDefinition();

	/**
	 * @return the itemType
	 */
	ItemType getItemType();

	boolean isLeaf();
	
	int getLevelIndex();
}