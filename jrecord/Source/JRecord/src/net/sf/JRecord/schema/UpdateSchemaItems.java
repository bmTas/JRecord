package net.sf.JRecord.schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.cgen.impl.ArrayFieldDefinition;
import net.sf.JRecord.schema.jaxb.Copybook;
import net.sf.JRecord.schema.jaxb.Item;


/**
 * Update a Cobol-Copybook items for use in
 * Generating / Reading Cobol Data files.
 * It is currently used to convert
 * <i>Cobol Data files</i> <b>to/from</b> <i>Xml</i>. 
 *
 * 
 * @author Bruce Martin
 *
 */
public class UpdateSchemaItems {
	
	public static final int D_NO_DUPLICATES = 1;
	public static final int D_NO_DUPLICATES_IN_RECORD = 2;
	public static final int D_DUPLICATES = 3;
	
    public static final int RO_LEAVE_ASIS = 0;
    public static final int RO_MINUS_TO_UNDERSCORE = 1; 
	public static final int RO_CAMEL_CASE = 2;
 
	
	private static final Map<String, Integer> EMPTY_RECORD_MAP = new HashMap<String, Integer>(0);
	
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
	private final Map<String, IArrayItemCheck> arrayChecks;
	
	private int duplicateFieldsStatus = -1;
	private final int varRenameOption;
	
	private IGetRecordFieldByName fieldLookup = null;
	//private List<Item> lastItems;
	private int itemCount = 0;
	private final List<Item> items;
	
	private int maxRecordLevel = Integer.MIN_VALUE;
	
	/**
	 * pdate a Cobol-Copybook items for use in
     * Generating / Reading Cobol Data files.
     * 
	 * @param copybook
	 * @param schema
	 * @param dropCopybook
	 * @param copybookName
	 * @param varRenameOption
	 */
	public UpdateSchemaItems(Copybook copybook, LayoutDetail schema, Map<String, IArrayItemCheck> arrayChecks, boolean dropCopybook, String copybookName, int varRenameOption) {
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
		this.varRenameOption = varRenameOption;
		this.items = copybook.getItem();
		this.arrayChecks = arrayChecks;

		
		duplicateFieldNames = schema.getDuplicateFieldNames();
		
		update(items, 0, -1, new ArrayList<String>(45));
		
	}
	
	private void update(List<Item> itemList, int indexs, int firstArraySize, ArrayList<String> levels) {
		 
		for (Item item : itemList) {
			String name = item.getName();
			name = name==null?"":name;
			String ucName = name.toUpperCase();
			boolean dup = duplicateFieldNames.contains(ucName);
			int newIndexs = indexs;
			item.names = new ArrayList<String>(levels);
			
			if (dropCopybook && name.length() > copybookName1.length()
			&& (ucName.startsWith(copybookName1) || ucName.startsWith(copybookName2))) {
				name = name.substring(copybookName1.length());
			}

			item.fieldName = name;
			item.nameToUse = updateName(name);

			levels.add(item.fieldName);
			if (item.getOccurs() != null && item.getOccurs() > 1) {
				newIndexs += 1;
				//System.out.println(item.getName() + " " + newIndexs);
				if (firstArraySize < 0) {
					firstArraySize = item.getOccurs();
				}
				
				if (ucName != null && ucName.length() > 0) {
					item.arrayCheck = arrayChecks.get(ucName);
				}
				arrayItems.put(item.nameToUse.toUpperCase(), item);
			}
			if (item.getItem().size() > 0) {
				update(item.getItem(), newIndexs, firstArraySize, levels);
			} else if ("filler".equalsIgnoreCase(name)) {
			} else if (newIndexs == 0) {
				item.itemType = Item.TYPE_FIELD;
				itemCount += 1;
				if (dup) {
					item.fieldDef = schema.getGroupField(levels.toArray(new String[levels.size()]));
				} else {
					item.fieldDef = schema.getFieldFromName(item.fieldName);
				}
			} else if (newIndexs > 4) {
				throw new RuntimeException("To many array indexs: " + newIndexs + ", only 1 to 4 are supported");
			} else {
				IFieldDetail[] fields = new IFieldDetail[newIndexs + 1];
				itemCount += 1;
				item.itemType = Item.TYPE_ARRAY;
				//System.out.println("\t" + item.getName() + " " + newIndexs);
				
				for (int i = 0; i < fields.length; i++) {
					levels.set(levels.size() - 1, item.fieldName + " " + ARRAY_INDEXS[newIndexs][i]);
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

	public final Map<String, Integer> getRecordHierarchyMap() {
		
		maxRecordLevel = -4;
		if (schema.hasTreeStructure()) {
			 Map<String, Integer> ret = new HashMap<String, Integer>();
			 int recIdx;
			 for (Item item : items) {
				 recIdx = schema.getRecordIndex(item.getName());
				 if (recIdx >= 0) {
					 int lvl = 0;
					 while ((recIdx = schema.getRecord(recIdx).getParentRecordIndex()) >= 0) {
						 lvl+= 1;
					 }
					 maxRecordLevel = Math.max(maxRecordLevel, lvl);
					 ret.put(item.nameToUse.toUpperCase(), lvl);
					 //System.out.println("\t == " + item.nameToUse.toUpperCase() + " " + lvl);
				 }
			 }
			 return ret;
		}
		return EMPTY_RECORD_MAP;
	}
	/**
	 * @return the maxRecordLevel
	 */
	public final int getMaxRecordHierarchyLevel() {
		if (maxRecordLevel == Integer.MIN_VALUE) {
			getRecordHierarchyMap();
		}
		return maxRecordLevel;
	}

	/**
	 * @return the duplicateFieldsStatus
	 */
	public final int getDuplicateFieldsStatus() {
		initDuplicateStatus();
		return duplicateFieldsStatus;
	}

	/**
	 * Check the status of duplicate Fields-Names,
	 * the rules are<ul>
	 * <li>If the duplicates all have the same position / length etc; they are
	 * essentially the same field and can be ignored. This is common
	 * when there is a <i>Record-Type</i> field in any record.
	 * <li>If the 'Duplicates are in different records, this can
	 * be handle using a Record / Field lookup</li>
	 * <li>Multiple fields in the same record can not be handled at present
	 * so throw an exception
	 * </ul>
	 */
	private void initDuplicateStatus() {
		if (duplicateFieldsStatus < 0) {
			duplicateFieldsStatus = D_NO_DUPLICATES;
			if (duplicateFieldNames.size() == 0) {
			} else if (schema.getRecordCount() == 1) {
				duplicateFieldsStatus = D_DUPLICATES;
			} else {
				for (String n : duplicateFieldNames) {
					IFieldDetail fld = null;
					for (int i = 0; i < schema.getRecordCount(); i++) {
						List<IFieldDetail> groupFields = schema.getRecord(i).getGroupFields(n);
						if (groupFields.size() == 1) {
							IFieldDetail f = groupFields.get(0);
							if (fld == null) {
								fld = f;
							} else {
								if (fld.getPos()  != f.getPos()
								||  fld.getLen()  != f.getLen()
								||  fld.getType() != f.getType()
								||  fld.getDecimal() != fld.getDecimal()
								) {
									duplicateFieldsStatus = D_NO_DUPLICATES_IN_RECORD;
									return;
								}
							}
						} else if (groupFields.size() > 1) {
							duplicateFieldsStatus = D_DUPLICATES;
							return;
						}
					}
				}
			}
		}
	}
	
	/**
	 * Reformat the Cobol name (if required) 
	 * @param name Cobol field name
	 * @return reformatted field name
	 */
	public String updateName(String name) {
		int l = name.length();
		StringBuilder b;
		char c;
		switch (varRenameOption) {
		case RO_CAMEL_CASE:
			boolean toUpper = false;
			String lcName = name.toLowerCase();
			String ucName = name.toUpperCase();
			b = new StringBuilder(name.length()) ;
			for (int i = 0; i < l; i++) {
				c = name.charAt(i);
				switch (c) {
				case '-':
				case ' ':
				case '_':
					toUpper = true;
					break;
				default:
					if (toUpper) {
						c = ucName.charAt(i);
					} else {
						c = lcName.charAt(i);
					}
					b.append(c);
					
					toUpper = false;
				}
			}
			name = b.toString();
			break;
		case RO_MINUS_TO_UNDERSCORE:
			b = new StringBuilder(name) ;
			for (int i = 0; i < l; i++) {
				switch (name.charAt(i)) {
				case '-':
				case ' ':
					b.setCharAt(i, '_');
					break;
				}
			}
			name = b.toString();
		}
		
		return name;
	}
	
	
	
	/**
	 * @return the fieldLookup
	 */
	public final IGetRecordFieldByName getFieldLookup() {
		if (fieldLookup == null) {
			fieldLookup = updateLookup(
					new FieldLookup(schema, itemCount, getDuplicateFieldsStatus() ),  
					items);
		}
		return fieldLookup;
	}
	
	private FieldLookup updateLookup(FieldLookup tl, List<Item> items) {
		for (Item item : items) {
			if (item.getItem().size() > 0) {
				updateLookup(tl, item.getItem()); 
			} else if (tl.recFields == null || ! tl.schema.getDuplicateFieldNames().contains(item.fieldName.toUpperCase())) {
				tl.fields.put(item.nameToUse.toUpperCase(), item);
			} else {
				tl.recFields.put((item.names.get(0) + "." + item.nameToUse).toUpperCase(), item);
			}
		}
		
		return tl;
	}

	private static class FieldLookup implements IGetRecordFieldByName {

		private final LayoutDetail schema;
		private final HashMap<String, Item> fields, recFields;
		
		private FieldLookup(LayoutDetail schema, int size, int dup) {
			this.schema = schema;
			
			this.fields = new HashMap<String, Item>(size * 2);
			
			HashMap<String, Item> t = null;
			if (dup == D_NO_DUPLICATES_IN_RECORD) {
				t = new HashMap<String, Item>();
			}
			recFields = t;
		}
		
		/* (non-Javadoc)
		 * @see net.sf.JRecord.cbl2xml.impl.IGetRecordFieldByName#getField(java.lang.String, java.lang.String)
		 */
		@Override
		public IFieldDetail getField(String recordName, String fieldName, int[] indexs) {
			String ucName = fieldName.toUpperCase();
			Item itm = fields.get(ucName);
			
			if (itm == null && recFields != null) {
				itm = recFields.get(recordName.toUpperCase() + '.' + ucName);
			}
			
			if (itm != null) {
				if (itm.fieldDef != null) {
					return itm.fieldDef;
				}
				if (indexs == null || indexs.length == 0) { return null; }
				
				StringBuilder b = new StringBuilder(itm.fieldName);
				String sep = " (";
				
				for (int i = 0; i < indexs.length; i++) {
					b.append(sep).append(indexs[i]);
					sep = ", ";
				}
				
				String n = b.append(')').toString();
				if (schema.getDuplicateFieldNames().contains(n.toUpperCase())) {
					return itm.arrayDef.getField(indexs);
				}
				return schema.getFieldFromName(n);
			}
			return null;
		}
		
	}
}
