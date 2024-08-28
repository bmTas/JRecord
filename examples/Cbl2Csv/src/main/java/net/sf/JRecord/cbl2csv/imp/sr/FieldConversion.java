package net.sf.JRecord.cbl2csv.imp.sr;

import net.sf.JRecord.Details.fieldValue.IFieldValue;

/**
 * This class contains common field conversion classes
 * @author Bruce Martin
 *
 */
public class FieldConversion {

	/**
	 * Normal field conversion. Program ends on the first error
	 */
	public static final IRetrieveFieldValue NORMAL_CONVERSION = new IRetrieveFieldValue() {
		@Override public String getFieldValue(int lineNumber, IFieldValue fieldValue) {
			return fieldValue.asString();
		}		
	};
	
	/**
	 * Replace field with conversion errors with ""
	 */
	public static final IRetrieveFieldValue FIELD_CONVERSION_REPLACE_ERRORS_WITH_NULL 
				= new RetrieveFieldValueErrorString("", false);

	/**
	 * Replace field with conversion errors with "", write a message to the System.err stream
	 */
	public static final IRetrieveFieldValue FIELD_CONVERSION_LOG_ERRORS_AND_REPLACE_WITH_NULL
				= new RetrieveFieldValueErrorString("", true);
	
	/**
	 * Create a field conversion class
	 * @param logErrors whether to write errors to the System.err stream or not
	 * @param errorText Text to write if there is a field conversion error
	 * @return
	 */
	public static IRetrieveFieldValue createFieldConversion(boolean logErrors, String errorText) {
		return new RetrieveFieldValueErrorString(errorText, logErrors);
	}

	private static class RetrieveFieldValueErrorString implements  IRetrieveFieldValue {
		private final String errorValue;
		private final boolean listError;
		
		
		public RetrieveFieldValueErrorString(String errorText, boolean logErrors) {
			super();
			this.errorValue = errorText;
			this.listError = logErrors;
		}


		@Override public String getFieldValue(int lineNumber, IFieldValue fieldValue) {
			try {
				return fieldValue.asString();
			} catch (Exception e) {
				if (listError) {
					System.err.print("Error line: " + lineNumber + " Field: " + fieldValue.getFieldDetail().getName());
					try {
						System.err.print( " Hex Value: " + fieldValue.asHex());
					} catch (Exception e1) {
					}
					System.err.println();
				}
				return errorValue;
			}
		}		

	}
}
