package net.sf.JRecord.detailsBasic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.External.Item.IItemJRec;
import net.sf.JRecord.cgen.def.IArrayExtended;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Item;

/**
 * Cobol Item Definition
 * @author bruce
 *
 */
public class ItemDtl extends Item implements IItemDetails {

	public static List<ItemDtl> EMPTY_LIST = Collections.unmodifiableList(new ArrayList<ItemDtl>(0));
	public static enum ItemType {
		
			GROUP(false, false, false),
			GROUP_ARRAY(false, true, false),
			GROUP_ARRAY_DEFINITION(false, true, true),
			FIELD(true, false, false),
			FIELD_ARRAY(true, true, false),
			FIELD_ARRAY_DEFINITION(true, true, true),
		;
		
		public final boolean isField, isArray, isArrayDefinition; 

		private ItemType(boolean isField, boolean isArray, boolean isArrayDefinition) {
			this.isField = isField;
			this.isArray = isArray;
			this.isArrayDefinition = isArrayDefinition; 
		}
	}
	private static ItemType[] itmTypeValues = ItemType.values();
		
	public static ItemType toItemType(boolean isField, boolean isArray, boolean isArrayDefinition) {
		int st = isField ? 3 : 0;
		
		for (int i = st; i < st + 3; i++) {
			if (itmTypeValues[i].isArray == isArray 
			&& itmTypeValues[i].isArrayDefinition == isArrayDefinition) {
				return itmTypeValues[i];
			}
		}
		
		throw new RuntimeException("Internal Error: unable to determine the ItemType ??? "
				+ isField + " " + isArray + " " + isArrayDefinition);
	}
	
	public final ItemType itemType;

	private final IFieldDetail fieldDefinition ;
	protected final IArrayExtended arrayDefinition;
//	private DependingOnDefinition.SizeField saveDtls, arraySizeField;

	private List<ItemDtl> childItems = EMPTY_LIST;
	public final int levelIndex;
	private int formatId;
	private String parameter, javaType; //JRecord Type Identifier

//	public ItemDtl(BaseItem parentItem, IItemDetails baseItem) {
//		this(	parentItem, baseItem, 
//				baseItem.getItemType().isArray, baseItem.getFieldDefinition(),
//				baseItem.getArrayDefinition(), baseItem.getLevelNumber(), false);
//	}
	
	public ItemDtl(
			BaseItem parentItem, IItemJRec baseItem, boolean isArray,
			IFieldDetail fieldDefinition, IArrayExtended arrayDefinition, int level) {
		this(parentItem, baseItem, isArray, fieldDefinition, arrayDefinition, level, true);
	}
	
	private ItemDtl(
			BaseItem parentItem, IItemJRec baseItem, boolean isArray,
			IFieldDetail fieldDefinition, IArrayExtended arrayDefinition, int level,
			boolean updateFieldDtl) {
		super(parentItem, baseItem.getLevelNumber(), baseItem.getLevelString(), baseItem.getFieldName());
		
		this.itemType = toItemType(
				baseItem.getChildItems() == null || baseItem.getChildItems().size() == 0,
				isArray,
				baseItem.getOccurs() >= 0);
		this.arrayDefinition = arrayDefinition;
		this.levelIndex = level;
		
		this.fieldDefinition = fieldDefinition;
		if (updateFieldDtl && fieldDefinition instanceof FieldDetail) {
			((FieldDetail) fieldDefinition).setCobolItem(this);
		}
		
		formatId = baseItem.getFormatId();
		parameter = baseItem.getParameter();
		super.set(baseItem);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#getChildItems()
	 */
	@Override
	public List<ItemDtl> getChildItems() {
		return childItems;
	}

	public void addItem(Item item) {
		addItem((ItemDtl) item);
	}

	public void addItem(ItemDtl item) {
		if (childItems == EMPTY_LIST) {
			childItems = new ArrayList<ItemDtl>(5);
		}
		childItems.add(item);
		add(item);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#getFieldDefinition()
	 */
	@Override
	public IFieldDetail getFieldDefinition() {
		return fieldDefinition;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#getArrayDefinition()
	 */
	@Override
	public IArrayExtended getArrayDefinition() {
		return arrayDefinition;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#getItemType()
	 */
	@Override
	public ItemType getItemType() {
		return itemType;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#isLeaf()
	 */
	@Override
	public boolean isLeaf() {
		return childItems == null || childItems.size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.IItemDetails#getLevelIndex()
	 */
	@Override
	public int getLevelIndex() {
		return levelIndex;
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
	
	public void setFormat(int formatId, String parameter) {
		this.formatId = formatId;
		this.parameter = parameter;
	}

	/**
	 * @return the javaType
	 */
	@Override
	public final String getJavaType() {
		return javaType;
	}

	/**
	 * @param javaType the javaType to set
	 */
	public final void setJavaType(String javaType) {
		this.javaType = javaType;
	}

}
