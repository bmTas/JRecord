package net.sf.JRecord.External.Def;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.JRecord.Common.IFieldDetail;


/**
 * Depending definitions; This is for JRecord internal use
 * 
 * @author Bruce Martin
 *
 */
public class DependingOnDefinition {
	public final List<DependingOn> dependOnList;
	int sizeFieldCount = 0;
	int moveableSizeFields = -1;
	public HashMap<Integer, SizeField> sizeFields;
	public ArrayList<SizeField> sizeFieldList;
	
	public DependingOnDefinition(List<DependingOn> dependOnList) {
		super();
		this.dependOnList = dependOnList;
		
		for (DependingOn c : dependOnList) {
			calcNumberOfSizeFields(new HashMap<String, Integer>(), c);
		}
	}
	
	private void calcNumberOfSizeFields(HashMap<String, Integer> fieldMap, DependingOn dependOn) {
		
		String key = dependOn.getVariableName().toLowerCase();
		Integer num = fieldMap.get(key);
		
		if (num == null) {
			num = (sizeFieldCount++);
			fieldMap.put(key, num);
		} 
		dependOn.fieldNumber = num;
		
		List<IDependingOnIndexDtls> indexDtls = dependOn.getIndexDtls();
		if (indexDtls != null) {
			for (IDependingOnIndexDtls id :indexDtls) {
				List<DependingOn> children = id.getChildren();
					if (children != null) {
					for (DependingOn c : children) {
						calcNumberOfSizeFields(fieldMap,  c);
					}
				}
			}
		}	
	}

	public int getSizeFieldCount() {
		return sizeFieldCount;
	}
	
	public Map<String, SizeField> getNameSizeFieldMap() {
		Map<String, SizeField> ret = new HashMap<String, SizeField>(sizeFieldCount * 2);
		
		for (SizeField sf : sizeFieldList) {
			if (sf != null && sf.name != null) {
				String key = sf.name.toLowerCase();
				if (! ret.containsKey(key)) {
					ret.put(key, sf);
				}
			}
		}
		
		return ret;
	}

	public int getMoveableSizeFields() {
		if (moveableSizeFields < 0) {
			moveableSizeFields = 0;
			buildSizeFieldMap();
		}
		return moveableSizeFields;
	}

	public final void buildSizeFieldMap() {
		
		if (sizeFields == null) {
			sizeFieldList = new ArrayList<SizeField>(sizeFieldCount+1);
			for (int i = sizeFieldCount; i >= 0; i--) {
				sizeFieldList.add(null);
			}
			checkForMovingFields(dependOnList, sizeFieldList);
			
			sizeFields = new HashMap<Integer, SizeField>(sizeFieldCount * 3 / 2);
			for (SizeField sf : sizeFieldList) {
				if (sf != null) {
					sizeFields.put(sf.position, sf);
				}
			}
		}
	}
	
	/**
	 * Get a size field base on its position
	 * 
	 * @param position
	 * 
	 * @return size field
	 */
	public SizeField getSizeField(int position) {
//		buildSizeFieldMap();
		return sizeFields.get(position);
	}

	private void checkForMovingFields(List<DependingOn> list, List<SizeField> sizeFields ) {
		if (list != null && list.size() > 0) {
			for (DependingOn c : list) {
				IFieldDetail field = c.getField();
				if (field.getPos() > dependOnList.get(0).getPosition()) {
					moveableSizeFields += 1;
				}

				if (sizeFields.get(c.fieldNumber) == null) {
					sizeFields.set(
							c.fieldNumber,
							new SizeField(
									field.getPos(), c.fieldNumber, 
									c.getVariableNameNoIndex(),
									! c.getVariableNameNoIndex().equals(c.getVariableName()))); 
				}

				List<IDependingOnIndexDtls> indexDtls = c.getIndexDtls();
				if (indexDtls != null) {
					for (IDependingOnIndexDtls id :indexDtls) {
						checkForMovingFields(id.getChildren(), sizeFields);
					}
				}
			}
		}
	}

	public static class SizeField {
		public final int position;
		public final int fieldNumber;
		public final String name;
		public final boolean indexedOD;

		private SizeField(int position, int fieldNumber, String name, boolean indexedOD) {
			super();
			this.position = position;
			this.fieldNumber = fieldNumber;
			this.name = name;
			this.indexedOD = indexedOD;
		}
	}
}
