package net.sf.JRecord.cgen.def.array;

public interface IArrayField2Dimension<FieldValue> {

	FieldValue get(int index1, int index2);

	int getArrayLength(int indexNumber);

}