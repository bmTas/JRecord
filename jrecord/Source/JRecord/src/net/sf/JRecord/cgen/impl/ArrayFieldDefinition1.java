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
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cgen.def.IArray1Dimension;
import net.sf.JRecord.cgen.def.IArray2Dimension;
import net.sf.JRecord.cgen.def.IArray3Dimension;
import net.sf.JRecord.cgen.def.IArrayExtended;
import net.sf.JRecord.cgen.def.IIndex;

public class ArrayFieldDefinition1 implements IArray1Dimension, IArray2Dimension, IArray3Dimension, IArrayExtended {

	private final int[]  lengths, numberOfElements;
	private FieldDetail[] fields;

	
	public ArrayFieldDefinition1(int[] arrayLengths) {
		this(arrayLengths, new FieldDetail[calcArraySize(arrayLengths)]);
	}
	
	private static int calcArraySize(int[] arrayLengths) {
		int size = 1;
		for (int aSize : arrayLengths) {
			size *= aSize;
		}
		return size;
	}

	public ArrayFieldDefinition1(int[] arrayLengths, FieldDetail[] fd) {

		this.lengths = arrayLengths;// new int[fd.length - 1];
		numberOfElements = new int[lengths.length];
		this.fields = fd;
		
		int num = 1;
		
		for (int i = numberOfElements.length-1; i >=0; i--) {
			numberOfElements[i] = num;
			num = num * (lengths[i]);
		}
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayExtended#setField(net.sf.JRecord.cgen.def.IIndex, net.sf.JRecord.Common.IFieldDetail)
	 */
	@Override
	public void setField(IIndex index, FieldDetail fieldDefinition) {
		this.fields[calcIndex(index)] = fieldDefinition;
	}
	


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayExtended#getField(net.sf.JRecord.cgen.def.IIndex)
	 */
	@Override
	public IFieldDetail getField(IIndex index) {
		return this.fields[calcIndex(index)];
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayAnyDimension#getFirstField()
	 */
	@Override
	public IFieldDetail getFirstField() {
		return this.fields[0];
	}

	private int calcIndex(IIndex index) {
		int idx = 0;
		
		for (int i = 0; i < numberOfElements.length; i++) {
			idx += index.getIndex(i) * numberOfElements[i];
		}
		return idx;
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
	@Override
	public IFieldDetail getField(int... indexs) {
		if (indexs.length != numberOfElements.length) { 
			throw new RecordException("You must supply " + numberOfElements.length + " indexs");
		}
		int idx = 0;
		
		for (int i = 0; i < indexs.length; i++) {
			idx += indexs[i] * numberOfElements[i];
		}
		
//		if (indexs.length > 1 && idx > lengths[lengths.length - 1] ) {
//			System.out.println(" --> " + idx + " " + indexs[0] + ", " + indexs[1] + " " + this.fields[idx].getName());
//		}
		
		return this.fields[idx];
	}
	
	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArray1Dimension#get(int)
	 */
	@Override
	public IFieldDetail get(int indexs) {
		return getField(indexs);
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

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayExtended#asOneDimensionArray()
	 */
	@Override
	public IArray1Dimension asOneDimensionArray() {
		if (lengths.length == 1) {
			return this;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayExtended#asTwoDimensionArray()
	 */
	@Override
	public IArray2Dimension asTwoDimensionArray() {
		if (lengths.length == 2) {
			return this;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IArrayExtended#asThreeDimensionArray()
	 */
	@Override
	public IArray3Dimension asThreeDimensionArray() {
		if (lengths.length == 3) {
			return this;
		}
		return null;
	}	
}
