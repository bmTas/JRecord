package net.sf.JRecord.cbl2xml.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
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

	private final Copybook copybook;
	private final LayoutDetail schema;
	private final Set<String> duplicateFieldNames;
	private final HashMap<String,Item> arrayItems = new HashMap<String, Item>();
	private final boolean dropCopybook;
	private final String copybookName;
	
	public ItemUpdater(Copybook copybook, LayoutDetail schema, boolean dropCopybook, String copybookName) {
		this.copybook = copybook;
		this.schema = schema;
		this.dropCopybook = dropCopybook;
		this.copybookName = copybookName;
				
		
		duplicateFieldNames = schema.getDuplicateFieldNames();
		
		update(copybook.getItem(), 0, new ArrayList<String>(45));
		
	}
	
	private void update(List<Item> items, int indexs, ArrayList<String> levels) {
		
		for (Item item : items) {
			String name = item.getName();
			String ucName = name.toUpperCase();
			boolean dup = duplicateFieldNames.contains(ucName);
			int newIndexs = indexs;
			item.names = new ArrayList<String>(levels);
			item.nameToUse = name;
			if (dropCopybook && ucName.startsWith(copybookName.toUpperCase())) {
				name = name.substring(copybookName.length() + 1);
				item.nameToUse = name;
			}
			levels.add(name);
			if (item.getOccurs() != null && item.getOccurs() > 1) {
				newIndexs += 1;
				System.out.println(item.getName() + " " + newIndexs);
				arrayItems.put(item.nameToUse.toUpperCase(), item);
			}
			if (item.getItem().size() > 0) {
				update(item.getItem(), newIndexs, levels);
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
				
				item.arrayDef = new ArrayFieldDefinition((RecordDetail)fields[0].getRecord(), fields);
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
