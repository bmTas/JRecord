package net.sf.JRecord.detailsBasic;

import java.util.ArrayList;

import net.sf.JRecord.cgen.def.IIndex;

/**
 * 
 * @author Bruce Martin
 *
 */
public class ArrayIndexDtls implements IIndex {

	public static final ArrayIndexDtls EMPTY = new EmptyArrayIndex();
//	public static final ArrayList<ArrayIndexDtls> EMPTY_ARRAY = new ArrayList<ArrayIndexDtls>(0);
	
	public final int occursMax;
	private int index = 0;
	private String indexStr;
	
	public final ArrayList<ArrayIndexDtls> indexList;
	public final boolean inArray;
	
	private int[] indexSizes;
	
	private ArrayIndexDtls() {
		occursMax = 0;
		indexList = new ArrayList<ArrayIndexDtls>(0);
		inArray  = false;
	}
	
	/**
	 * 
	 * @param prevIndexs
	 * @param occursMax
	 */
	public ArrayIndexDtls(ArrayIndexDtls prevIndexs, int occursMax) {
		this.occursMax = occursMax;
		this.inArray = true;
		if (prevIndexs == null || prevIndexs.indexList.size() ==0) {
			indexList = new ArrayList<ArrayIndexDtls>(1);
		} else {
			indexList = new ArrayList<ArrayIndexDtls>(prevIndexs.indexList.size() + 1);
			indexList.addAll(prevIndexs.indexList);
		}
		indexList.add(this);
	}

	public int getNumberOfIndexs() {
		return indexList.size();
	}
	
	public int getIndex(int idx) {
		return indexList.get(idx).index;
	}

	/**
	 * @return the index
	 */
	public int getIndex() {
		return index;
	}

	public void incIndex() {
		setIndex(index + 1);
	}
	
	
	/**
	 * @param index the index to set
	 */
	public void setIndex(int index) {
		this.index = index;
		this.indexStr = Integer.toString(index);
	}
	
	public int[] toIndexSizeArray() {
		if (indexSizes == null) {
			indexSizes = new int[indexList.size()];
			for (int i = 0; i < indexSizes.length; i++) {
				indexSizes[i] = indexList.get(i).occursMax;
			}
		}
		return indexSizes;
	}
	public String toIndexStr() {

		StringBuilder b = new StringBuilder();
		
		b.append(indexList.get(0).indexStr);
		for (int i = 1; i < indexList.size(); i++) {
			b.append(',')
			 .append(' ')
			 .append(indexList.get(i).indexStr);
		}

		return b.toString();
	}
	
	public static class EmptyArrayIndex extends ArrayIndexDtls {


		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsBasic.ArrayIndexDtls#setIndex(int)
		 */
		@Override
		public void setIndex(int index) {
			if (index != 0) {
				throw new RuntimeException("You can not update the index for the EMPTY Array Index");
			}
			super.setIndex(index);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsBasic.ArrayIndexDtls#toIndexStr()
		 */
		@Override
		public String toIndexStr() {
			return "";
		}
		
	}
}
