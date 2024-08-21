package net.sf.JRecord.cgen.def.array;

public interface IArrayField3Dimension<FieldValue> {

	FieldValue get(int index1, int index2, int index3);

	int getArrayLength(int indexNumber);

}