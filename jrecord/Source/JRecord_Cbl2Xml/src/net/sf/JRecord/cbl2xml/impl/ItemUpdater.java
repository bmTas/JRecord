package net.sf.JRecord.cbl2xml.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.cbl2xml.jaxb.Copybook;
import net.sf.JRecord.cbl2xml.jaxb.Item;
import net.sf.JRecord.cgen.impl.ArrayFieldDefinition;

public class ItemUpdater {
	
	private static final String[][] ARRAY_INDEXS = {
		{"", ""},
		{"(1)", "(0)"},
		{"(1, 0)", "(0, 1)", "(0, 0)"},
		{"(1, 0, 0)", "(0, 1, 0)", "(0, 0, 1)", "(0, 0, 0)"},
		{"(1, 0, 0, 0)", "(0, 1, 0, 0)", "(0, 0, 1, 0)", "(0, 0, 0, 1)", "(0, 0, 0, 0)"},
	};

	//private final Copybook copybook;
	private final LayoutDetail schema;
	private final Set<String> duplicateFieldNames;
	private final HashMap<String,Item> arrayItems = new HashMap<String, Item>();
	private final boolean dropCopybook;
	private final String copybookName1, copybookName2;
	
	public ItemUpdater(Copybook copybook, LayoutDetail schema, boolean dropCopybook, String copybookName) {
		//this.copybook = copybook;
		this.schema = schema;
		if (copybookName == null || copybookName.length() == 0) {
			dropCopybook = false;
			copybookName = "";
		}
		this.dropCopybook = dropCopybook;
		copybookName = copybookName.toUpperCase();
		this.copybookName1 = copybookName + "-";
		this.copybookName2 = copybookName + "_";
				
		
		duplicateFieldNames = schema.getDuplicateFieldNames();
		
		update(copybook.getItem(), 0, -1, new ArrayList<String>(45));
		
	}
	
	private void update(List<Item> items, int indexs, int firstArraySize, ArrayList<String> levels) {
		
		for (Item item : items) {
			String name = item.getName();
			name = name==null?"":name;
			String ucName = name.toUpperCase();
			boolean dup = duplicateFieldNames.contains(ucName);
			int newIndexs = indexs;
			item.names = new ArrayList<String>(levels);
			item.nameToUse = name;
			if (dropCopybook && name.length() > copybookName1.length()
			&& (ucName.startsWith(copybookName1) || ucName.startsWith(copybookName2))) {
				name = name.substring(copybookName1.length());
				item.nameToUse = name;
			}
			levels.add(name);
			if (item.getOccurs() != null && item.getOccurs() > 1) {
				newIndexs += 1;
				//System.out.println(item.getName() + " " + newIndexs);
				if (firstArraySize < 0) {
					firstArraySize = item.getOccurs();
				}
				arrayItems.put(item.nameToUse.toUpperCase(), item);
			}
			if (item.getItem().size() > 0) {
				update(item.getItem(), newIndexs, firstArraySize, levels);
			} else if (newIndexs == 0) {
				item.itemType = Item.TYPE_FIELD;
				if (dup) {
					item.fieldDef = schema.getGroupField(levels.toArray(new String[levels.size()]));
				} else {
					item.fieldDef = schema.getFieldFromName(item.nameToUse);
				}
			} else if (newIndexs > 4) {
				throw new RuntimeException("To many array indexs: " + newIndexs + ", only 1 to 4 are supported");
			} else {
				IFieldDetail[] fields = new IFieldDetail[newIndexs + 1];
				item.itemType = Item.TYPE_ARRAY;
				System.out.println("\t" + item.getName() + " " + newIndexs);
				
				for (int i = 0; i < fields.length; i++) {
					levels.set(levels.size() - 1, name + " " + ARRAY_INDEXS[newIndexs][i]);
					fields[i] = schema.getGroupField(levels.toArray(new String[levels.size()]));
				}
				
				item.arrayDef = new ArrayFieldDefinition((RecordDetail)fields[0].getRecord(), firstArraySize, fields);
			}
			levels.remove(levels.size() - 1);
		}
	}

	/**
	 * @return the arrayItems
	 */
	public final Map<String, Item> getArrayItems() {
		return arrayItems;
	}
}
