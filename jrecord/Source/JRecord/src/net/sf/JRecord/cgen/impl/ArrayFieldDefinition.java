package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.cgen.def.IArray1Dimension;
import net.sf.JRecord.cgen.def.IArray2Dimension;
import net.sf.JRecord.cgen.def.IArray3Dimension;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;

public class ArrayFieldDefinition implements IArray1Dimension, IArray2Dimension, IArray3Dimension, IArrayAnyDimension {

	private final int[] sizeAdj, lengths;
	private final IFieldDetail firstField;
	private final RecordDetail record;
	private final DependingOnDtls dependingOnDtls;
	private DependingOnDtls lastDependingOnDtls;
	
	public ArrayFieldDefinition(RecordDetail rec, int firstArrayLength, IFieldDetail... fd) {
		this.record = rec;
		sizeAdj = new int[fd.length - 1];
		lengths = new int[fd.length - 1];
		firstField =  fd[sizeAdj.length];
		lengths[0] = firstArrayLength;
		sizeAdj[0] = fd[0].getPos() - firstField.getPos();
		for (int i = 1; i < sizeAdj.length; i++) {
			sizeAdj[i] = fd[i].getPos() - firstField.getPos();
			lengths[0] = sizeAdj[i - 1] / sizeAdj[i];
		}
		

		DependingOnDtls depOn = null;
		if (firstField instanceof FieldDetail) {
			depOn = ((FieldDetail) firstField).getDependingOnDtls();
		}
		dependingOnDtls = depOn;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArray3Dimension#get(int, int, int)
	 */
	@Override
	public IFieldDetail get(int index1, int index2, int index3) {
		return getField(index1, index2, index3);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArray2Dimension#get(int, int)
	 */
	@Override
	public IFieldDetail get(int index1, int index2) {
		return getField(index1, index2);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArray1Dimension#get(int)
	 */
	@Override
	public IFieldDetail get(int indexs) {
		return getField(indexs);
	}

	@Override
	public IFieldDetail getField(int... indexs) {
		if (indexs == null || indexs.length != sizeAdj.length) {
			throw new RuntimeException("Expected: " + sizeAdj.length + " indexes, but recieved: "
					+ (indexs == null? 0 : indexs.length));
		}
		int p = indexs[0] * sizeAdj[0] + firstField.getPos();
		StringBuilder b = new StringBuilder(firstField.getName())
								.append(" (")
								.append(Integer.toString((indexs[0] + 1)));
		for (int i = 1; i < sizeAdj.length; i++) {
			p += indexs[i] * sizeAdj[i]; 
			b.append(", ").append(Integer.toString((indexs[i] + 1)));
		}
		b.append(")");
	
		
		FieldDetail f =  FieldDetail.newFixedWidthField(
				b.toString(), firstField.getType(), 
				p, firstField.getLen(), firstField.getDecimal(), firstField.getFontName());
		f.setRecord(record);
		f.setDependingOnDtls(getDependingOnDtls(indexs));
		
		return f;
	}
	
	private DependingOnDtls getDependingOnDtls(int[] indexs) {
		if (dependingOnDtls == null || cmp(dependingOnDtls, indexs)) {
			return dependingOnDtls;
		} else if (lastDependingOnDtls == null || (! cmp(lastDependingOnDtls, indexs))) {
			lastDependingOnDtls = bld(dependingOnDtls, indexs.length - 1, indexs);
		} 
		return lastDependingOnDtls;
	}
	
	private DependingOnDtls bld(DependingOnDtls d, int lvl, int[] indexs) {
		if (lvl < 0 || d == null) { 
			return null;
		} 
		
		return new DependingOnDtls(d.dependingOn, indexs[lvl], bld(d, lvl-1, indexs));
	}
	
	private boolean cmp(DependingOnDtls d, int[] indexs) {
		for (int i = indexs.length- 1; i >= 0; i--) {
			if (d.index != indexs[i]) {
				return false;
			}
			d = d.parent;
		}
		return true;
	}
	
	/**
	 * get the Array length
	 * @param indexNumber array index number
	 * @return array length
	 */
	@Override
	public int getArrayLength(int indexNumber) {
		return lengths[indexNumber];
	}
}
