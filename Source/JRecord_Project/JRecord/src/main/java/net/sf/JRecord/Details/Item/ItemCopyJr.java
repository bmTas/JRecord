package net.sf.JRecord.Details.Item;

import java.util.ArrayList;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.External.Item.IItemJRec;
import net.sf.JRecord.External.base.FieldCreatorHelper;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.cgen.def.IArrayExtended;
import net.sf.JRecord.cgen.impl.ArrayFieldDefinition1;
import net.sf.JRecord.detailsBasic.ArrayIndexDtls;
import net.sf.JRecord.detailsBasic.ItemCopy;
import net.sf.JRecord.detailsBasic.ItemDtl;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.IItemJr;


public class ItemCopyJr extends ItemCopy {
	
	final boolean excludeFillers;
	ArrayList<FieldDetail> fields = new ArrayList<FieldDetail>();
	private FieldDetail lastFiller, lastField;
	private final TypeManager.CharsetType charsetType;
	private final boolean optermizeType;

	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.ItemCopy#createField(net.sf.JRecord.External.base.FieldCreatorHelper, int, net.sf.JRecord.detailsBasic.ItemDtl, net.sf.JRecord.detailsBasic.ArrayIndexDtls, net.sf.JRecord.External.Def.DependingOnDtls, int)
	 */
	public ItemCopyJr(boolean includeFillers, TypeManager.CharsetType charsetType, boolean optermizeType) {
		super();
		this.excludeFillers = ! includeFillers;
		this.charsetType = charsetType;
		this.optermizeType = optermizeType;
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.ItemCopy#addFieldToList(net.sf.JRecord.Common.IFieldDetail)
	 */
	@Override
	protected void addFieldToList(IFieldDetail fieldDetail) {
		String name = fieldDetail.getName();
		if ((!excludeFillers) 
		|| (name != null && name.length() > 0 && !"filler".equalsIgnoreCase(name))) {
			fields.add((FieldDetail)fieldDetail);
		}
	}




	@Override
	protected FieldDetail createField(FieldCreatorHelper fieldHelper, int level, IItemJRec itm,
			ArrayIndexDtls arrayIndexDtls, DependingOnDtls dependOnParentDtls, int basePos,
			boolean addToList) {


		FieldDetail fd; 
		String fieldName = itm.getFieldName();
		String fldName = fieldHelper.createFieldName(fieldName, arrayIndexDtls.toIndexStr());
		
		int type = fix(itm.getType());
        if (optermizeType) {
        	type = TypeManager.getInstance().getShortType(type, itm.getStorageLength(), charsetType);
        }

		if (itm.getChildItems() != null && itm.getChildItems().size() > 0) {
			fd = createField(fieldHelper, Type.ftChar, dependOnParentDtls, level, itm, arrayIndexDtls, fldName, basePos);
		} else if (excludeFillers 
			   && (fieldName == null || fieldName.length() == 0 || "filler".equalsIgnoreCase(fieldName))) {
			fd = createField(fieldHelper, type, dependOnParentDtls, level, itm, arrayIndexDtls, fldName, basePos);
			
			if (lastFiller == null || lastFiller.getEnd() < fd.getEnd()) {
				lastFiller = fd;
			}
		} else {
			fd = createField(fieldHelper, type, dependOnParentDtls, level, itm, arrayIndexDtls, fldName, basePos);

			if (addToList) {
				fields.add(fd);
			}

			if (lastField == null || lastField.getEnd() < fd.getEnd()) {
				lastField = fd;
			}
		}
		
		return fd;
	}


	/**
	 * @param fieldHelper
	 * @param level
	 * @param itm
	 * @param arrayIndexDtls
	 * @param fieldName
	 * @return
	 */
	private FieldDetail createField(FieldCreatorHelper fieldHelper, int type, DependingOnDtls dependOnParentDtls, int level,
			IItemJr itm,
			ArrayIndexDtls arrayIndexDtls, String fieldName, int basePos) {

		FieldDetail fd = FieldDetail.newFixedWidthField(
				fieldName, type, basePos + fix(itm.getPosition()), 
				fix(itm.getStorageLength()), 
				fieldHelper.calculateDecimalSize(type, itm.getPicture(), itm.getScale()),
				fieldHelper.getFont());
		if (level > 1 && fieldHelper.getGroupNameSize() > level) {
//			if (fieldName != null && fieldName.startsWith("TRANSACTION-DATE")) {
//				System.out.println(level + "\t" + fieldName + "\t" + fieldHelper.getGroupName(level - 1));
//			}
		   	fd.setGroupName(fieldHelper.getGroupName(level - 1));
		}
		fd.setDependingOnDtls(dependOnParentDtls);
		
		if (arrayIndexDtls.inArray && itm instanceof ItemDtl) {
			ItemDtl cobolItem = ((ItemDtl) itm);
			fd.setCobolItem(cobolItem);
			cobolItem.getArrayDefinition().setField(arrayIndexDtls, fd);
		}
		return fd;
	}
	
	private int fix(int val) {
		return val == IItem.NULL_INT_VALUE ? 0 : val;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsBasic.ItemCopy#createArray(net.sf.cb2xml.def.IItem, net.sf.JRecord.detailsBasic.ArrayIndexDtls)
	 */
	@Override
	protected IArrayExtended createArray(IItemJr item, ArrayIndexDtls idxDtls) {
		return idxDtls.getNumberOfIndexs() > 0 
				? new ArrayFieldDefinition1(idxDtls.toIndexSizeArray())
				: null;
	}
	
	public FieldDetail[] getFields() {
		if (lastFiller != null) {
			if (lastField == null || lastFiller.getEnd() > lastField.getEnd()) {
				fields.add(lastFiller);
			}
			lastFiller = null;
		}
		return fields.toArray(new FieldDetail[fields.size()]);
	}
}
