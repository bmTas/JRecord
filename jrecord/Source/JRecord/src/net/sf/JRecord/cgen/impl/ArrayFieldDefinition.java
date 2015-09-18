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

	private final int[] sizeAdj;
	private final IFieldDetail firstField;
	private final RecordDetail record;
	private final DependingOnDtls dependingOnDtls;
	
	public ArrayFieldDefinition(RecordDetail rec, IFieldDetail... fd) {
		this.record = rec;
		sizeAdj = new int[fd.length - 1];
		firstField =  fd[sizeAdj.length];
		for (int i = 0; i < sizeAdj.length; i++) {
			sizeAdj[i] = fd[i].getPos() - firstField.getPos();
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
		f.setDependingOnDtls(dependingOnDtls);
		
		return f;
	}
}
