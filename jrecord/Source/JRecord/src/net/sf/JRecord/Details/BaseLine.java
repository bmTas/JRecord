package net.sf.JRecord.Details;

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
     * @param type type to use when getting the field
     * @param field field to retrieve
     *
     * @return fields Value
     * 
     */
    public abstract Object getField(int type, IFieldDetail field);

//	@Override
	public IFieldValue getFieldValue(IFieldDetail field) {
		return new FieldValue(this, field);
	}

	@Override
	public  IFieldValue getFieldValue(int recordIdx, int fieldIdx) {
		return new FieldValue(this, recordIdx, fieldIdx);
	}

	@Override
	public final IFieldValue getFieldValue(String fieldName) {
		IFieldDetail fieldFromName = layout.getFieldFromName(fieldName);
		
		if (fieldFromName == null) {
			throw new RuntimeException("Field: \"" + fieldName +"\" does not exist !!!");
		}
		
		return  getFieldValue(fieldFromName);
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.AbstractLine#getFieldValueIfExists(java.lang.String)
	 */
	@Override
	public IFieldValue getFieldValueIfExists(String fieldName) {
		return  getFieldValue(layout.getFieldFromName(fieldName));
	}

	/**
	 * Get Field Iterator for the requested Record-Type
	 * @param recordName Record Name to retrieve the field list for.
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
