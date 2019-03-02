package net.sf.JRecord.detailsBasic;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.External.Item.IItemJRec;
import net.sf.JRecord.External.base.FieldCreatorHelper;
import net.sf.JRecord.External.base.IAddDependingOn;
import net.sf.JRecord.cgen.def.IArrayExtended;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.IItemJr;

public abstract class ItemCopy implements IAddDependingOn {

	private ArrayList<DependingOn> dependingOn = new ArrayList<DependingOn>(6);

	
	public List<ItemDtl> copy(FieldCreatorHelper fldHelper, List<? extends IItemJRec> items) {
		List<ItemDtl> list = null;
		if (items != null) {
			list = copy(fldHelper, null, false, items, ArrayIndexDtls.EMPTY, fldHelper.getInitialLevel());
			loadItems(fldHelper, list, ArrayIndexDtls.EMPTY, null, fldHelper.getInitialLevel(), 0);
		}
		return list;
	}
	
	private List<ItemDtl> copy(
			FieldCreatorHelper fldHelper, Item parent, boolean isArray,
			List<? extends IItemJRec> items, ArrayIndexDtls idxDtls,
			int level) {
		ArrayList<ItemDtl> itemDtls = null;
		if (items != null) {
			itemDtls = new ArrayList<ItemDtl>();
			for (IItemJRec itm : items) {
				if (itm.getLevelNumber() != 88) {
					boolean isArrayItm = isArray || itm.getOccurs() >= 0; 
					ArrayIndexDtls newIdxDtls = itm.getOccurs() >= 0 
										? new ArrayIndexDtls(idxDtls, itm.getOccurs())
										: idxDtls;
					IFieldDetail fld = null;
					fldHelper.updateGroup(level, itm.getFieldName());
					if (! newIdxDtls.inArray) {
						fld = createField(fldHelper, level, itm, idxDtls, null, 0, false);
						//FieldCreatorHelper fieldHelper, int level, ItemDtl itm, 
						//ArrayIndexDtls arrayIndexDtls, DependingOnDtls dependOnParentDtls,
						//int basePos);

					}
					ItemDtl itemDtl = createItem(parent, level, itm, isArrayItm, newIdxDtls, fld);
					copy(fldHelper,	itemDtl, isArrayItm, itm.getChildItems(),
							newIdxDtls, level+1);
					itemDtls.add(itemDtl);
				}
			}
		}

		return itemDtls;
	}

	/**
	 * @param parent
	 * @param level
	 * @param itm
	 * @param isArrayItm
	 * @param newIdxDtls
	 * @param fld
	 * @return
	 */
	protected ItemDtl createItem(Item parent, int level, IItemJRec itm, boolean isArrayItm, ArrayIndexDtls newIdxDtls,
			IFieldDetail fld) {
		ItemDtl itemDtl = new ItemDtl(parent, itm, isArrayItm, 
				fld, createArray(itm, newIdxDtls), level);
		if (fld != null) {
			itemDtl.setType(fld.getType());
		}
		return itemDtl;
	}
	
	private void loadItems(
			FieldCreatorHelper fldHelper, 
			List<ItemDtl> itms, ArrayIndexDtls idxDtls,
			DependingOnDtls dependOnParentDtls,
			int level,
			int basePos) {

		for (ItemDtl itm  : itms) {
			if (itm.getLevelNumber() != 88) {
				List<ItemDtl> childItems = itm.getChildItems();
				int numChildren = childItems.size();
				String dependingVar = itm.getDependingOn();
//				if (itm.getFieldName() != null && itm.getFieldName().equals("TRANSACTION-DATE")) {
//					System.out.print('.');
//				}
				fldHelper.updateGroup(level, itm.getFieldName());
				
//				if (itm.getFieldName() != null && itm.getFieldName().startsWith("TRANSACTION-DATE")) {
//					System.out.println(level + "\t" + fldHelper.getGroupName(level - 1) + "\t: " + itm.getFieldName());
//				}				
				
				if (numChildren == 0 && itm.getFieldDefinition() != null) {
					addFieldToList(itm.getFieldDefinition());
				}
				if (itm.getOccurs() > 0) {
                    DependingOn dependOn = null;
                    
                    if (dependingVar != null && dependingVar.length() > 0) {
                    	dependOn = fldHelper
                    					.dependingOnBuilder()
                    						.setPosition(itm.getPosition() + basePos)
                    						.setLength(itm.getStorageLength())
                    						.setChildOccurs(itm.getOccurs())
                    					.newDependingOn(this, dependOnParentDtls, dependingVar);
                    }
                    
                    ArrayIndexDtls newIdxDtls = new ArrayIndexDtls(idxDtls, itm.getOccurs());
					if (numChildren == 0) {
						for (int i = 0; i < itm.getOccurs(); i++) {
							newIdxDtls.setIndex(i);
//							if (itm.getFieldName() != null && "array-fld".equals(itm.getFieldName())) {
//								System.out.println(">> " + itm.getFieldName() + " : " + newIdxDtls.toIndexStr());
//							}
							itm.arrayDefinition.setField(
									newIdxDtls,
									createField(
											fldHelper, level, itm, 
											newIdxDtls, // problem area
											createDependingOnDtls(dependOn, dependOnParentDtls, i),
											basePos + i * itm.getStorageLength(),
											true)
									);
						}						
					} else {
						for (int i = 0; i < itm.getOccurs(); i++) {
							newIdxDtls.setIndex(i);
							loadItems(
									fldHelper, childItems, 
									newIdxDtls,
									createDependingOnDtls(dependOn, dependOnParentDtls, i),
									level+1,
									basePos + i * itm.getStorageLength());
						}
					}		
				} else if (itm.getOccurs() == 0) {
				} else if (numChildren == 0) {
					if (idxDtls.inArray) {
						itm.arrayDefinition.setField(
								idxDtls, 
								createField(fldHelper, level, itm, idxDtls, dependOnParentDtls, basePos, true));
					}
				} else {
					loadItems(fldHelper, childItems, idxDtls, dependOnParentDtls, level+1, basePos);
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.base.IAddDependingOn#addDependingOn(net.sf.JRecord.External.Def.DependingOn)
	 */
	@Override
	public final void addDependingOn(DependingOn child) {
		dependingOn.add(child);
	}


	/**
	 * @return the dependingOn
	 */
	public final ArrayList<DependingOn> getDependingOn() {
		return dependingOn;
	}

	private DependingOnDtls createDependingOnDtls(DependingOn dependOn, DependingOnDtls dependOnParentDtls, int idx) {
        DependingOnDtls dependOnDtls = dependOnParentDtls;
        if (dependOn != null) {
        	dependOnDtls = new DependingOnDtls(dependOn, idx, dependOnParentDtls);
        }
        return dependOnDtls;
	}

	protected abstract void addFieldToList(IFieldDetail fieldDetail);
	
	protected abstract FieldDetail createField(
			FieldCreatorHelper fieldHelper, int level, IItemJRec itm, 
			ArrayIndexDtls arrayIndexDtls, DependingOnDtls dependOnParentDtls,
			int basePos, boolean addToList);
	
	protected abstract IArrayExtended createArray(IItemJr item, ArrayIndexDtls idxDtls);
}
