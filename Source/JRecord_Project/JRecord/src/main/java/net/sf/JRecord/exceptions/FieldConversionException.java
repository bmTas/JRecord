package net.sf.JRecord.exceptions;

import net.sf.JRecord.Common.FieldConversionError;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;;

@SuppressWarnings("serial")
public class FieldConversionException extends FieldConversionError {

	private final AbstractLine line;
	
	public FieldConversionException(AbstractLine line, IFieldDetail field, String msg, Throwable exception) {
		super(field, msg, exception);
		this.line = line;
	}

	public FieldConversionException(FieldConversionError conversionError, AbstractLine line) {
		super(conversionError.getField(), conversionError.getMessage(), conversionError.getCause());
		this.line = line;
	}
	
	
	/**
	 * @return the line
	 */
	public AbstractLine getLine() {
		return line;
	}


}
