package net.sf.JRecord.Details;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;

public abstract class BasicLine extends BaseLine implements AbstractLine {

	protected static final byte[] NULL_RECORD = new byte[0];
	protected LineProvider lineProvider;
	protected int preferredLayoutAlt = Constants.NULL_INTEGER;
	protected int preferredLayout = Constants.NULL_INTEGER;
	protected int writeLayout = Constants.NULL_INTEGER;

	public BasicLine(LineProvider defaultProvider, LayoutDetail linesLayout) {
		super();

		lineProvider = defaultProvider;
		layout = linesLayout;
	}


	/**
	 * Get the field value as Hex
	 *
	 * @param recordIdx Index of the current layout used to retrieve the field
	 * @param fieldIdx Index of the current field
	 *
	 * @return field value as a Hex String
	 */
	public final String getFieldHex(final int recordIdx, final int fieldIdx) {

		try {
			IFieldDetail field = layout.getField(recordIdx, fieldIdx);

			return layout.getField(getData(),
			        				Type.ftHex,
			        				field).toString();

		} catch (final Exception ex) {
			return "";
		}
	}


	/**
	 * @param pLayout The layouts to set.
	 */
	public void setLayout(final LayoutDetail pLayout) {
		this.layout = pLayout;
		preferredLayoutAlt = Constants.NULL_INTEGER;
	}


	/**
	 * Alternative get layout method without length checks
	 */
	@Override
	public int getPreferredLayoutIdxAlt() {
		if (preferredLayoutAlt == Constants.NULL_INTEGER) {
			int defaultIdx = Constants.NULL_INTEGER;
			int i = 0;
			int defCount = -1;
			//RecordDetail rec;
			RecordSelection sel;
			int size = layout.getRecordCount();

			if (size == 1) {
			    preferredLayoutAlt = 0;
			} else if (layout.getDecider() != null) {
			    preferredLayoutAlt = layout.getDecider().getPreferedIndex(this);
			}


	    	//System.out.println();
			while ((i < size) && (preferredLayoutAlt == Constants.NULL_INTEGER)) {
				sel = layout.getRecord(i).getRecordSelection();
				switch (sel.isSelected(this)) {
				case DEFAULT:
					if (sel.size() > defCount) {
						defaultIdx = i;
						defCount = sel.size();
					}
					break;

				case YES:
					preferredLayoutAlt = i;
					break;
				}

				i += 1;
			}
			if (preferredLayoutAlt == Constants.NULL_INTEGER) {
				preferredLayoutAlt = defaultIdx;
			}
		}

		return preferredLayoutAlt;
	}



	/**
	 * @param pWriteLayout The writeLayout to set.
	 */
	public void setWriteLayout(final int pWriteLayout) {
		this.preferredLayoutAlt = pWriteLayout;
		this.writeLayout = pWriteLayout;
	}

	/**
	 * Gets a fields value
	 *
	 * @param recordIdx Index of the RecordDescription to be used.
	 * @param fieldIdx Index of the required field
	 *
	 * @return the request field (formated)
	 */
	public Object getField(final int recordIdx, final int fieldIdx) {
		try {
			if (fieldIdx == Constants.FULL_LINE) {
		        return getFullLine();
			}

			return getField(layout.getField(recordIdx, fieldIdx));

		} catch (final Exception ex) {
			ex.printStackTrace();
			return "";
		}
	}

	/**
	 * Get a fields value
	 *
	 * @param fieldName field to retrieve
	 *
	 * @return fields Value
	 */
	public Object getField(String fieldName) {
		IFieldDetail fld = layout.getFieldFromName(fieldName);

	   	if (fld == null) {
	   		return null;
	   	}

	   	return getField(fld);
	}


    /**
     * Test if Tree rebuild is required
     */
	public boolean isRebuildTreeRequired() {
		return false;
	}

	/**
	 * Set a field via its name
	 *
	 * @param fieldName fieldname to be updated
	 * @param value value to be applied to the field
	 *
	 * @throws RecordException any conversion error
	 */
	public void setField(String fieldName, Object value) throws RecordException {
		IFieldDetail fld = layout.getFieldFromName(fieldName);

		if (fld != null) {
			setField(fld, value);
		}
	}

	/**
	 * Sets a field to a new value
	 *
	 * @param recordIdx record layout
	 * @param fieldIdx field number in the record
	 * @param val new value
	 *
	 * @throws RecordException any error that occurs during the save
	 */
	public void setField(final int recordIdx, final int fieldIdx, Object val)
			throws RecordException {

	    IFieldDetail field = layout.getField(recordIdx, fieldIdx);

	    //adjustLengthIfNecessary(field, recordIdx);

	   	setField(field, val);
	}

	/**
     * Set the line provider
     *
     * @param pLineProvider The lineProvider to set.
     */
    public void setLineProvider(LineProvider pLineProvider) {
        this.lineProvider = pLineProvider;
    }
}