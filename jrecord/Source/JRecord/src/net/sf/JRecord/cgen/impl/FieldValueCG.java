package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.BaseFieldValue;
import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 * This class Retrieves a Field Value from an array of bytes
 * Its for use in CodeGen generated java classes
 * 
 * @author Bruce Martin
 *
 */
public class FieldValueCG extends BaseFieldValue {

	private final IGetByteData dataSource;
	private final IFieldDetail fieldDetails;
	private final Type type;
	
	/**
	 * This class Retrieves a Field Value from an array of bytes
     * Its for use in CodeGen generated java classes
     * 
	 * @param dataSource source of dataLine (as an array of bytes)
	 * @param fieldDetails field definition
	 */
	public FieldValueCG(IGetByteData dataSource, IFieldDetail fieldDetails) {
		super(fieldDetails);
		
		this.dataSource = dataSource;
		this.fieldDetails = fieldDetails;
		this.type = TypeManager.getInstance().getType(fieldDetails.getType());
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#getValue()
	 */
	@Override
	protected Object getValue() {
		return type.getField(dataSource.getData(), fieldDetails.getPos(), fieldDetails);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.BaseFieldValue#set(java.lang.Object)
	 */
	@Override
	public void set(Object value) {
		dataSource.setData(type.setField(dataSource.getData(), fieldDetails.getPos(), fieldDetails, value));
	}
}
