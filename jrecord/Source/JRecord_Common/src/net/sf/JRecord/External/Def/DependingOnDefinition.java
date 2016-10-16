package net.sf.JRecord.External.Def;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

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

	public int getMoveableSizeFields() {
		if (moveableSizeFields < 0) {
			moveableSizeFields = 0;
			buildSizeFieldMap();
		}
		return moveableSizeFields;
	}

	public final void buildSizeFieldMap() {
		
		if (sizeFields == null) {
			ArrayList<SizeField> sizeFldList = new ArrayList<SizeField>(sizeFieldCount+1);
			for (int i = sizeFieldCount; i >= 0; i--) {
				sizeFldList.add(null);
			}
			checkForMovingFields(dependOnList, sizeFldList);
			
			sizeFields = new HashMap<Integer, SizeField>(sizeFieldCount * 3 / 2);
			for (SizeField sf : sizeFldList) {
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
					sizeFields.set(c.fieldNumber, new SizeField(field.getPos(), field.getLen()));
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

		public SizeField(int position, int fieldNumber) {
			super();
			this.position = position;
			this.fieldNumber = fieldNumber;
		}
	}
}
