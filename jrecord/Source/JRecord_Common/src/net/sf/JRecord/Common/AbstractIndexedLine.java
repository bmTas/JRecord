package net.sf.JRecord.Common;

public interface AbstractIndexedLine {

	/**
	 * Gets a fields value
	 *
	 * @param recordIdx Index of the RecordDescription to be used.
	 * @param fieldIdx Index of the required field
	 *
	 * @return the request field (formated)
	 */
	public abstract Object getField(final int recordIdx, final int fieldIdx);

	/**
	 * Get a fields value
	 *
	 * @param field field to retrieve
	 *
	 * @return fields Value
	 *
	 */
	public abstract Object getField(IFieldDetail field);

	   /**
     * Get the Preferred Record Layout Index for this record
     *
     * @return Index of the Record Layout based on the Values
     */
    public abstract int getPreferredLayoutIdx();

	//
	//    /**
	//     * Get a fields value
	//     *
	//     * @param fieldName field to retrieve
	//     *
	//     * @return fields Value
	//     *
	//     * @deprecated use getFieldValue
	//     */
	//    public abstract Object getField(String fieldName);

	/**
	 * Sets a field to a new value
	 *
	 * @param recordIdx record layout
	 * @param fieldIdx field number in the record
	 * @param val new value
	 *
	 * @throws RecordException any error that occurs during the save
	 */
	public abstract void setField(final int recordIdx, final int fieldIdx,
			Object val) throws RecordException;

	/**
	 * Set a fields value
	 *
	 * @param field field to retrieve
	 * @param value value to set the field to
	 *
	 * @throws RecordException any error that occurs
	 */
	public abstract void setField(IFieldDetail field, Object value)
			throws RecordException;

}