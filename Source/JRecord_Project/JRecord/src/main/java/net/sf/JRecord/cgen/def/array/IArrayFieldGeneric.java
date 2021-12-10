package net.sf.JRecord.cgen.def.array;

public interface IArrayFieldGeneric<FieldValue> {

	FieldValue getField(int... indexs);

	int getArrayLength(int indexNumber);

	int getIndexCount();

}