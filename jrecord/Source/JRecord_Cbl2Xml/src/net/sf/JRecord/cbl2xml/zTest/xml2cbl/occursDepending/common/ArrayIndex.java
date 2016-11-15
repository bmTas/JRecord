package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

public class ArrayIndex {
	private int[] indexs = new int[10];
	private int numberOfIndexs = 0;
	

	public void incNumberOfIndexs() {
		indexs[numberOfIndexs++] = 0;
	}
	

	public void incIndex() {
		indexs[numberOfIndexs-1] += 1;
	}


	public int getCurrentIndex() {
		return indexs[numberOfIndexs-1];
	}

	public int getIndex(int idxNum) {
		return indexs[idxNum];
	}

	/**
	 * @return the numberOfIndexs
	 */
	public final int getNumberOfIndexs() {
		return numberOfIndexs;
	}


	public void decNumberOfIndexs() {
		numberOfIndexs -= 1; 
	}
	
	public int getValue(int num) {
		int ret = 0;
		for (int i = 0; i < numberOfIndexs; i++) {
			ret = (ret + indexs[i]) * num;
		}
		
		return ret;
	}

	public String toString() {
		if (numberOfIndexs == 0) { return ""; }
		
		StringBuilder b = new StringBuilder(numberOfIndexs * 3 + 5 );
		String sep = " (";
		for (int i = 0; i < numberOfIndexs; i++) {
			b.append(sep).append(indexs[i]);
			sep = ", ";
		}
		
		return b.append(')').toString();
	}
//	private int getIndex(int idx) {
//		return indexs[idx];
//	}

}
