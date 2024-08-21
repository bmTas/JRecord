/**
 * 
 */
package net.sf.JRecord.cgen.support;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cgen.def.IArray1Dimension;
import net.sf.JRecord.cgen.def.IArray2Dimension;
import net.sf.JRecord.cgen.def.IArray3Dimension;
import net.sf.JRecord.cgen.def.IArrayStdDef;

/**
 * @author Bruce
 *
 */
public class ArrayGenField implements IArrayStdDef {

	private final String fieldName;
	private final int  position, length, type, decimal;
	private final String font;
	private final ArraySizeDef[] arrayDimensions;
	
	
	
	public ArrayGenField(String fieldName, int position, int length, int type, int decimal, String font,
			ArraySizeDef... arrayDimensions) {
		super();
		this.fieldName = fieldName;
		this.position = position;
		this.length = length;
		this.type = type;
		this.decimal = decimal;
		this.font = font;
		this.arrayDimensions = arrayDimensions;
	}

	public int getPosition() {
		return position;
	}

	@Override
	public IFieldDetail get(int index) {
		return getField(index);
	}

	@Override
	public int getArrayLength() {
		return getArrayLength(0);
	}

	@Override
	public int getArrayLength(int indexNumber) {
		return arrayDimensions[indexNumber].arraySize;
	}

	@Override
	public IFieldDetail get(int index1, int index2) {
		return getField(index1, index2);
	}

	@Override
	public IFieldDetail get(int index1, int index2, int index3) {
		return getField(index1, index2, index3);
	}

	@Override
	public IFieldDetail getField(int... indexs) {
		if (indexs.length != arrayDimensions.length) { 
			throw new RecordException("You must supply " + arrayDimensions.length + " indexs");
		}
		int pos = position;
		StringBuilder b = new StringBuilder(fieldName.length() + 4 * (indexs.length + 1)).append(fieldName);
		String sep = " (";
		for (int i = 0; i < indexs.length; i++) {
			pos += indexs[i] * arrayDimensions[i].inc;
			b.append(sep).append(indexs[i]);
			sep = ", ";
		}
		return FieldDetail.newFixedWidthField(b.append(')').toString(), type, pos, length, decimal, font);
	}

	@Override
	public IFieldDetail getFirstField() {
		return getField(new int[arrayDimensions.length]);
	}

	@Override
	public int getArrayElementSize(int indexNumber) {
		return 0;
	}

	@Override
	public int getIndexCount() {
		return arrayDimensions.length;
	}

	@Override
	public IArray1Dimension asOneDimensionArray() {
		return this;
	}

	@Override
	public IArray2Dimension asTwoDimensionArray() {
		return this;
	}

	@Override
	public IArray3Dimension asThreeDimensionArray() {
		return this;
	}

	public static class ArraySizeDef {
		public final int arraySize, inc;

		public ArraySizeDef(int arraySize, int inc) {
			super();
			this.arraySize = arraySize;
			this.inc = inc;
		}
		
	}
}
