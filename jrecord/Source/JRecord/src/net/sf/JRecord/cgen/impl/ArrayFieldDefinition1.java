/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.cgen.def.IArray1Dimension;
import net.sf.JRecord.cgen.def.IArray2Dimension;
import net.sf.JRecord.cgen.def.IArray3Dimension;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;

public class ArrayFieldDefinition1 implements IArray1Dimension, IArray2Dimension, IArray3Dimension, IArrayAnyDimension {

	//private final int[] sizeAdj, lengths, indexPos;
	private final int[]  lengths, numberOfElements;
	//private final FieldDetail firstField;
	private final RecordDetail record;
	private final FieldDetail[] fields;

	
	public ArrayFieldDefinition1(RecordDetail rec, int[] arrayLengths, FieldDetail[] fd) {
		this.record = rec;
		//sizeAdj = new int[fd.length - 1];
		this.lengths = arrayLengths;// new int[fd.length - 1];
		numberOfElements = new int[lengths.length];
		this.fields = fd;
		
		int num = 1;
		
		for (int i = numberOfElements.length-1; i >=0; i--) {
			numberOfElements[i] = num;
			num = num * (lengths[i]);
		}
		
//		firstField =  (FieldDetail) fd[sizeAdj.length];
//		lengths[0] = firstArrayLength;
//		sizeAdj[0] = fd[0].getPos() - firstField.getPos();
//		
//		if (firstField.getIndexOfField() < 0) {
//			throw new RuntimeException("Internal Error index of Field < 0");
//		}
//		for (int i = 1; i < sizeAdj.length; i++) {
//			sizeAdj[i] = fd[i].getPos() - firstField.getPos();
//			lengths[i] = sizeAdj[i - 1] / sizeAdj[i];
//			if (fd[i].getIndexOfField() < 0) {
//				throw new RuntimeException("Internal Error index of Field < 0");
//			}
//			indexPos[i] = fd[i].getIndexOfField() - firstField.getIndexOfField(); 
//		}
		

	}
//	
//	public ArrayFieldDefinition1(RecordDetail rec, String name, int pos, int size, int[] maxIdx, int[] elementSizes) {
//		this.record = rec;
//		sizeAdj = new int[size];
//		lengths = new int[size];
//		dependingOnDtls = null;
//		firstField = FieldDetail.newFixedWidthField(name + " (0)", Type.ftChar, pos, elementSizes[size - 1], 0, "");
//		for (int i = 0; i < size; i++) {
//			sizeAdj[i] = elementSizes[i];
//			lengths[i] = maxIdx[i];
//		}
//	}

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
		int idx = 0;
		
		for (int i = 0; i < indexs.length; i++) {
			idx += indexs[i] * numberOfElements[i];
		}
		
//		if (indexs.length > 1 && idx > lengths[lengths.length - 1] ) {
//			System.out.println(" --> " + idx + " " + indexs[0] + ", " + indexs[1] + " " + this.fields[idx].getName());
//		}
		
		return this.fields[idx];
	}
	
//	private DependingOnDtls getDependingOnDtls(int[] indexs) {
//		if (dependingOnDtls == null || cmp(dependingOnDtls, indexs)) {
//			return dependingOnDtls;
////		} else if (lastDependingOnDtls == null || (! cmp(lastDependingOnDtls, indexs))) {
////			lastDependingOnDtls = bld(dependingOnDtls, indexs.length - 1, indexs);
////		} 
////		return lastDependingOnDtls;
//		}
//		return bld(dependingOnDtls, indexs.length - 1, indexs);
//	}
//	
//	private DependingOnDtls bld(DependingOnDtls d, int lvl, int[] indexs) {
//		if (lvl < 0 || d == null) { 
//			return null;
//		} 
//		
//		return new DependingOnDtls(d.dependingOn, indexs[lvl], bld(d, lvl-1, indexs), false);
//	}
//	
//	private boolean cmp(DependingOnDtls d, int[] indexs) {
//		for (int i = indexs.length- 1; i >= 0 && d != null; i--) {
//			if (d.index != indexs[i]) {
//				return false;
//			}
//			d = d.parent;
//		}
//		return true;
//	}
//	
	/**
	 * get the Array length
	 * @param indexNumber array index number
	 * @return array length
	 */
	@Override
	public int getArrayLength(int indexNumber) {
		return lengths[indexNumber];
	}

	@Override
	public int getArrayElementSize(int indexNumber) {
		return lengths[indexNumber] * this.fields[0].getLen();
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayAnyDimension#getIndexCount()
	 */
	@Override
	public int getIndexCount() {
		return lengths.length;
	}
	
	
}
