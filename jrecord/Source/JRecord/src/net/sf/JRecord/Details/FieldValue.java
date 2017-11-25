package net.sf.JRecord.Details;

import net.sf.JRecord.Common.IFieldDetail;


/**
 * FieldValue class provided for backward compatibility with CodeGen
 * 
 * @author Bruce Martin
 * 
 * @deprecated use {@link net.sf.JRecord.Details.fieldValue.FieldValue} instead !!!
 *
 */
@Deprecated
public class FieldValue extends net.sf.JRecord.Details.fieldValue.FieldValue {

	/**
	 * @deprecated use {@link net.sf.JRecord.Details.fieldValue.FieldValue} instead !!!
	 */
	@Deprecated
	public FieldValue(AbstractLine line, IFieldDetail fieldDetails) {
		super(line, fieldDetails);
	}

	/**
	 * @deprecated use {@link net.sf.JRecord.Details.fieldValue.FieldValue} instead !!!
	 */
	@Deprecated
public FieldValue(AbstractLine line, int recordIndex, int fieldIndex) {
		super(line, recordIndex, fieldIndex);
	}

}
