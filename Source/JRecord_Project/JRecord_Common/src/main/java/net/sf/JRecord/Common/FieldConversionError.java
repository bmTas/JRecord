package net.sf.JRecord.Common;


/**
 * Basic Field Conversion error
 * 
 * @author Bruce Martin
 *
 */
@SuppressWarnings("serial")
public class FieldConversionError extends RecordException {

	private final IFieldDetail field;

	
	public FieldConversionError(IFieldDetail field, String msg, Throwable exception) {
		super(msg, exception);
		this.field = field;
	}

	/**
	 * @return the field
	 */
	public IFieldDetail getField() {
		return field;
	}

}
