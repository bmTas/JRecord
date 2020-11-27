/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.Details;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.detailsSelection.RecordSelection;

public abstract class BasicLine extends BaseLine {

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

			return getField(Type.ftHex, field).toString();

		} catch (final Exception ex) {
			return "";
		}
	}


	/**
	 * @param pLayout The layouts to set.
	 * @deprecated was for use in the RecordEditor, do not use in JRecord
	 */
	@Override
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
					
				case NO:
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
	 * 
	 * @param recordIdx The recordIndex to be used calculate size (when writing the record)
	 */
	public void setRecordIdxForOutput(int recordIdx) {
		setWriteLayout(recordIdx);
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
	 */
	public void setField(String fieldName, Object value) {
		IFieldDetail fld = layout.getFieldFromName(fieldName);

		if (fld != null) {
			setField(fld, value);
		}
	}

	   /**
     * Set a fields value
     *
     * @param field field to retrieve
     * @param value value to set the field to
     *
      */
    public final void setField(IFieldDetail field, Object value)
    {
        setField(field.getType(), field, value);
    }
    
    /**
     * Set the fields type (overriding the type on the Field)
     * @param type type-identifier to use
     * @param field Field-definition
     * @param value new field value
     * 
     */
    protected abstract void setField(int type, IFieldDetail field, Object value);

	/**
	 * Sets a field to a new value
	 *
	 * @param recordIdx record layout
	 * @param fieldIdx field number in the record
	 * @param val new value
	 *
	 */
	public void setField(final int recordIdx, final int fieldIdx, Object val) {

	    IFieldDetail field = layout.getField(recordIdx, fieldIdx);

	    //adjustLengthIfNecessary(field, recordIdx);

	   	setField(field, val);
	}
	
	/**
	 * Check for Occurs depending size field update
	 * 
	 * @param field field being updated
	 */
	protected final void checkForOdUpdate(IFieldDetail field) {
		if (field != null) {
			for (int i = 0; i < layout.getRecordCount(); i++) {
				layout.getRecord(i).checkForSizeFieldUpdate(this, field);
			}		
		}
	}
	
	protected final void clearOdBuffers() {
		for (int i = 0; i < layout.getRecordCount(); i++) {
			layout.getRecord(i).clearOdBuffers(this);
		}		
	}

	/**
     * Set the line provider
     *
     * @param pLineProvider The lineProvider to set.
     */
    public void setLineProvider(LineProvider pLineProvider) {
        this.lineProvider = pLineProvider;
    }
    
	/**
	 * Check wether a field is defined in the record
	 * @param recIdx record Index
	 * @param fldIdx field Index
	 * @return wether the field is defined
	 */
	public final boolean isDefined(int recIdx, int fldIdx) {
		return isDefined(layout.getRecord(recIdx).getField(fldIdx));
	}
	
}