package net.sf.JRecord.Details;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;

public abstract class BaseLine implements AbstractLine {

	protected LayoutDetail layout;


	/**
     * Get a fields value
     *
     * @param field field to retrieve
     *
     * @return fields Value
     */
	@Override
    public final Object getField(IFieldDetail field) {
        return getField(field.getType(), field);
    }

    /**
     * Get a fields value
     *
     * @param record record containg the field
     * @param type type to use when getting the field
     * @param field field to retrieve
     *
     * @return fields Value
     * 
     */
    public abstract Object getField(int type, IFieldDetail field);

//	@Override
	public final AbstractFieldValue getFieldValue(IFieldDetail field) {
		return new FieldValue(this, field);
	}

	@Override
	public final AbstractFieldValue getFieldValue(int recordIdx, int fieldIdx) {
		return new FieldValue(this, recordIdx, fieldIdx);
	}

	@Override
	public final AbstractFieldValue getFieldValue(String fieldName) {
		return  getFieldValue(layout.getFieldFromName(fieldName));
	}



	/**
	 * Get Field Iterator for the requested Record-Type
	 * @param recordNumber Record Name
	 * @return Field Iterator
	 */
	@Override public final FieldIterator getFieldIterator(String recordName) {
		int recordNumber = layout.getRecordIndex(recordName);
		if (recordNumber < 0) {
			throw new RuntimeException("Record: " + recordName + " does not exist in layout");
		}
		return new FieldIterator(this, recordNumber);
	}


	/**
	 * Get Field Iterator for the requested Record-Type
	 * @param recordNumber record number
	 * @return Field Iterator
	 */
	@Override public final FieldIterator getFieldIterator(int recordNumber) {
		return new FieldIterator(this, recordNumber);
	}

	/**
	 * Get the Layout
	 * @return Returns the layouts.
	 */
	public LayoutDetail getLayout() {
	    return layout;
	}
}
