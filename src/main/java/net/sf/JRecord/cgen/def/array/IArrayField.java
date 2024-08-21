package net.sf.JRecord.cgen.def.array;

import net.sf.JRecord.Details.fieldValue.IFieldAttributes;

public interface IArrayField<FieldValue extends IFieldAttributes>
		extends IArrayField1Dimension<FieldValue>, IArrayField2Dimension<FieldValue>,
				IArrayField3Dimension<FieldValue>, IArrayFieldGeneric<FieldValue> {

}
