package net.sf.JRecord.cgen.impl.array;

import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.Details.fieldValue.BaseFieldValue;
import net.sf.JRecord.Details.fieldValue.IBigIntegerField;
import net.sf.JRecord.Details.fieldValue.IDecimalField;
import net.sf.JRecord.Details.fieldValue.IIntField;
import net.sf.JRecord.Details.fieldValue.ILongField;
import net.sf.JRecord.Details.fieldValue.IStringField;
import net.sf.JRecord.cgen.def.IArrayStdDef;
import net.sf.JRecord.cgen.def.array.IArrayField;
import net.sf.JRecord.cgen.impl.fields.FieldValueCG;

public class ArrayFieldValue  {

	private final IArrayStdDef arrayFieldDef;

	private final IGetByteData dataSource;
	
	
	public ArrayFieldValue(IArrayStdDef arrayFieldDef, IGetByteData dataSource) {
		super();
		this.arrayFieldDef = arrayFieldDef;
		this.dataSource = dataSource;
	}

	public FieldValueCG get(int index1, int index2, int index3) {
		return getField(index1, index2, index3);
	}


	public FieldValueCG get(int index1, int index2) {
		return getField(index1, index2);
	}


	public FieldValueCG get(int index) {
		return getField(index);
	}


	public FieldValueCG getField(int... indexs) {
		return new FieldValueCG(dataSource, arrayFieldDef.getField(indexs));
	}


	public BaseFieldValue getFirstField() {
		return new FieldValueCG(dataSource, arrayFieldDef.getFirstField());
	}

	public int getArrayLength(int indexNumber) {
		return arrayFieldDef.getArrayLength(indexNumber);
	}

	public int getIndexCount() {
		return arrayFieldDef.getIndexCount();
	}

	/**
	 * integer array Field
	 * @author Bruce Martin
	 *
	 */
	public static class IntArrayField extends ArrayFieldValue implements IArrayField<IIntField> {
		/**
		 * Integer array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public IntArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}
	
	/**
	 * long array Field
	 * @author Bruce Martin
	 *
	 */
	public static class LongArrayField extends ArrayFieldValue implements IArrayField<ILongField> {
		/**
		 * Long array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public LongArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}

	
	/**
	 * Decimal array Field
	 * @author Bruce Martin
	 *
	 */
	public static class DecimalArrayField extends ArrayFieldValue implements IArrayField<IDecimalField> {
		/**
		 * Decimal array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public DecimalArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}
	
	
	/**
	 * Decimal array Field
	 * @author Bruce Martin
	 *
	 */
	public static class BigIntegerArrayField extends ArrayFieldValue implements IArrayField<IBigIntegerField> {
		/**
		 * BigInteger array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public BigIntegerArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}

	/**
	 * String array Field
	 * @author Bruce Martin
	 *
	 */
	public static class StringArrayField extends ArrayFieldValue implements IArrayField<IStringField> {
		/**
		 * String array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public StringArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}

	/**
	 * General purpose array Field
	 * @author Bruce Martin
	 *
	 */
	public static class GeneralArrayField extends ArrayFieldValue implements IArrayField<FieldValueCG> {
		/**
		 * General purpose array Field
		 * @param arrayFieldDef Array Definition
		 * @param dataSource Data source
		 */
		public GeneralArrayField(IArrayStdDef arrayFieldDef, IGetByteData dataSource) { super(arrayFieldDef, dataSource); }
	}

}
