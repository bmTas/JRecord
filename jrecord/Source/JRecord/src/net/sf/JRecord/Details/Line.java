/*
 * Created on 29/05/2004
 *
 * This class represents one line (or Record) in the File. It contains
 * methods to Get / update fields (for a specified layouts) get the
 * prefered layout etc
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - remove unused fields
 */
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

import java.math.BigInteger;
import java.util.Arrays;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeChar;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;

/**
 * This class represents one line (or Record) in the File. It contains
 * methods to Get / update fields (for a specified layouts) get the
 * preferred layout etc
 *
 * <p>The one important method is getFieldValue
 *
 * <p>Creating:
 * <pre>
 *              <font color="brown"><b>Line</b></font> outLine = new Line(oLayout);
 * </pre>
 *
 * <p>Getting a field value:
 * <pre>
 *              <font color="brown"><b>long</b></font> sku = saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").asLong();
 * </pre>
 *
 * <p>Updating a field:
 * <pre>
 *              saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").set(1331);
 * </pre>
 *
 * @author Bruce Martin
 * @version 0.55
 */
public class Line extends BasicLine implements AbstractLine, IGetByteData {

	static LineProvider defaultProvider = new DefaultLineProvider();

	byte[] data;

	int preferredLayoutAlt = Constants.NULL_INTEGER;
	private boolean newRecord   = false;


	/**
	 * Define a Null record
	 *
	 * @param group - Group of Record Layouts
	 */
	public Line(final LayoutDetail group) {
		super(defaultProvider, group);

		newRecord = true;
		data = NULL_RECORD;
	}



	/**
	 * Define a String record
	 *
	 * @param group - Group of Record Layouts
	 * @param rec - record
	 */
	public Line(final LayoutDetail group, final String rec) {
		super(defaultProvider, group);

		data = Conversion.getBytes(rec, group.getFontName());
	}


	/**
	 * Define a Byte record
	 *
	 * @param group - Group of Record Layouts
	 * @param rec - record
	 */
	public Line(final LayoutDetail group, final byte[] rec) {
		super(defaultProvider, group);

		layout = group;

		data = rec;
	}


	/**
	 * Create  a line from a selected part of a supplied byte array
	 *
	 * @param group current group of records
	 * @param buf input buffer
	 * @param start start of the record (in the input buffer)
	 * @param recordLen Record (or Line length)
	 */
	public Line(final LayoutDetail group, final byte[] buf,
	        	final int start, final int recordLen) {
		super(defaultProvider, group);

		data = new byte[Math.max(0, recordLen)];

		layout = group;

		if (recordLen > 0) {
		    System.arraycopy(buf, start, data, 0, recordLen);
		}
	}


	/**
	 *   This method completely replaces a lines value. It is used to determine
	 * a records prefered record layout
	 *
	 * @param rec buffer holding the record
	 * @param start Start of the record
	 * @param len length of the record
	 */
	@Override
	public final void replace(final byte[] rec, final int start, final int len) {

		System.arraycopy(rec, start, data, 0,
					java.lang.Math.min(len, data.length));
		super.preferredLayoutAlt = Constants.NULL_INTEGER;
		super.preferredLayout = Constants.NULL_INTEGER;
		clearOdBuffers();
	}





	/**
	 * Get the field values as raw Text
	 *
	 * @param recordIdx Index of the current layout used to retrieve the field
	 * @param fieldIdx Index of the current field
	 *
	 * @return field value (raw Text)
	 */
	public String getFieldText(final int recordIdx, final int fieldIdx) {

		try {
			if (fieldIdx == Constants.FULL_LINE) {
				return getFullLine();
			}

		    FieldDetail field = layout.getField(recordIdx, fieldIdx);
			return getField(Type.ftChar, field).toString();

		} catch (final Exception ex) {
			return "";
		}
	}

	/**
	 * Get the full line as text
	 *
	 * @return line as text
	 */
	public String getFullLine() {
	    String s;

	    if ("".equals(layout.getFontName())) {
	        s = new String(data);
	    } else {
	        try {
	            s = new String(data, layout.getFontName());
	        } catch (Exception e) {
	            s = new String(data);
            }
	    }

	    return s;
	}


	public byte[] getFieldBytes(final int recordIdx, final int fieldIdx) {
	    return getFieldBytes(layout.getField(recordIdx, fieldIdx));
	}

	public byte[] getFieldBytes(IFieldDetail field) {
	    int len = field.getLen();
	    if (field.getType() == Type.ftCharRestOfRecord) {
	    	len = data.length - field.calculateActualPosition(this);
	    }

	    return getData(field.calculateActualPosition(this), len);
	}

	/**
	 * Get the Prefered Record Layout Index for this record
	 *
	 * @return Index of the Record Layout based on the Values
	 */
	@Override
	public int getPreferredLayoutIdx() {
		int ret = preferredLayout;

		if (ret == Constants.NULL_INTEGER) {
			ret = getPreferredLayoutIdxAlt();

			if (ret < 0) {
				for (int i=0; i< layout.getRecordCount(); i++) {
					if (this.data.length == layout.getRecord(i).getLength()) {
						ret = i;
						preferredLayout = i;
						break;
					}
				}
			}
		}

		return ret;
	}




	/**
	 * Adjust the record length if required
	 *
	 * @param field field being updated
	 * @param recordIdx current record layout index
	 */
	private void adjustLengthIfNecessary(final IFieldDetail field, final int recordIdx) {

		if (field.calculateActualEnd(this) > data.length) {
			RecordDetail record = layout.getRecord(recordIdx);
			if (record == null) {
			} else if (record.hasDependingOn()) {
				int end = Math.max(field.calculateActualEnd(this), layout.getRecord(recordIdx).getMinumumPossibleLength());

				if (field != null) {
					ensureCapacity(end);
				}
			} else {
				adjustLength(recordIdx);
			}
		}
	}



	/**
	 * @param field
	 * @param end
	 */
	private void ensureCapacity(int end) {
		if (end > data.length) {
		    newRecord(Math.max(end, layout.getMinimumRecordLength()));
		    if (writeLayout >= 0
		    && writeLayout < layout.getRecordCount()
		    && layout.getRecord(writeLayout).getLength() < end) {
		    	writeLayout = -1;
		    }
		}
	}


	/**
	 * Adjust the record length
	 *
	 * @param recordIdx Layout index
	 */
	private void adjustLength(final int recordIdx) {

		RecordDetail pref = layout.getRecord(recordIdx);
		int newSize = pref.getLength(); //field.getEnd();

		if (newSize != data.length && ! pref.hasDependingOn()) {
			if (newRecord) {
				this.writeLayout = recordIdx;
				this.preferredLayoutAlt = recordIdx;
				this.preferredLayout = recordIdx;
			}

			newRecord(newSize);
		}
	}


	/**
	 * Resize the record
	 *
	 * @param newSize new record size
	 */
	private void newRecord(int newSize) {
//		byte[] sep = layout.getRecordSep();
		byte[] rec = new byte[newSize];
		int len = Math.min(rec.length, data.length);

		newRecord = false;
		System.arraycopy(data, 0, rec, 0, len);
		
		if (len < rec.length && layout.getInitByte() != 0) {
			Arrays.fill(rec, len, rec.length, layout.getInitByte());
		}
		
//		if ((layout.getLayoutType() == Constants.rtGroupOfBinaryRecords)
//				&& sep != null && sep.length > 0) {
//			System.arraycopy(sep, 0, rec, newSize - sep.length, sep.length);
//		}

		data = rec;
	}

	public byte[] getData(int start, int len) {
	    byte[] temp = getData();
	    byte[] ret;
	    int tempLen = Math.min(len, temp.length - start + 1);

	    if (temp.length < start || tempLen < 1) {
	        return null;
	    }

	    ret = new byte[tempLen];
	    System.arraycopy(temp, start - 1, ret, 0, tempLen);

	    return ret;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IGetByteData#getData()
	 */
	@Override
	public byte[] getData() {

		if (newRecord && (writeLayout >= 0)) {
			adjustLength(writeLayout);
		}
		return data;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IGetByteData#setData(byte[])
	 */
	@Override
	public void setData(byte[] data) {
		replaceDataValue(data);
	}



	private void replaceDataValue(byte[] data) {
		this.data = data;
		super.preferredLayoutAlt = Constants.NULL_INTEGER;
		super.preferredLayout = Constants.NULL_INTEGER;
		clearOdBuffers();
	}



	public final void setData(String newVal) {
		replaceDataValue(Conversion.getBytes(newVal, layout.getFontName()));
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
    @SuppressWarnings("deprecation")
	public Object getField(int type, IFieldDetail field) {

    	//System.out.print(" ---> getField ~ 1");
        if (field.isFixedFormat()) {
            int position = field.calculateActualPosition(this);
           
			return TypeManager.getSystemTypeManager().getType(type) //field.getType())
					.getField(this.getData(), position, field);
        }

        return layout.getCsvField(this.getData(), type, field);
    }



    
    /**
     * Set the fields type (overriding the type on the Field)
     * @param type type-identifier to use
     * @param field Field-definition
     * @param value new field value
     * 
     */
    public void setField(int type, IFieldDetail field, Object value) {
    	
        if (field.isFixedFormat()) {
            int pos = field.calculateActualPosition(this);
//            if (pos < 0) {
//            	pos = field.calculateActualPosition(this);
//            }
            
//            if (field.calculateActualEnd(this) == 101) {
//            	 System.out.println("~~ " + field.calculateActualEnd(this));
//            }
            ensureCapacity(field.calculateActualEnd(this));
//            System.out.println("~~ " + field.calculateActualEnd(this));
			data = TypeManager.getSystemTypeManager().getType(type)
				.setField(getData(), pos, field, value);
			
			super.checkForOdUpdate(field);
        } else  {
            data = layout.setCsvField(getData(), type, field, value);
        }
    }


    /**
     * Update field without appling any formatting
     *
     * @param recordIdx record layout
	 * @param fieldIdx field number in the record
	 * @param value new value
	 *
     */
    public void setFieldText(final int recordIdx, final int fieldIdx, String value) {

        FieldDetail field = layout.getField(recordIdx, fieldIdx);

        adjustLengthIfNecessary(field, recordIdx);
        setField(Type.ftChar, field, value);
    }

    /**
     * Set a field to a Hex value
     * @param recordIdx record index
     * @param fieldIdx field index
     * @param val hex value
     */
	public String setFieldHex(final int recordIdx, final int fieldIdx,
	        String val) {
		FieldDetail field = layout.getField(recordIdx, fieldIdx);
		adjustLengthIfNecessary(field, recordIdx);
 	 	return setFieldHex(field, val);
	}
	 
	public String setFieldHex(IFieldDetail field, String val) {
	    String ret = null;

	    ensureCapacity(field.calculateActualEnd(this));

        try {
            int i, j;
            BigInteger value = new BigInteger(val, Type.BASE_16);
            byte[] bytes = value.toByteArray();

            j = field.calculateActualEnd(this) - 1;
            int start = field.calculateActualPosition(this) - 1;
            int en = Math.max(0, bytes.length - (val.length() + 1) / 2);
			for (i = bytes.length - 1; i >= en && j >= start; i--) {
                data[j--] = bytes[i];
            }
            for (i = j; i >= start; i--) {
                data[i] = 0;
            }
            super.checkForOdUpdate(field);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RecordException("Error saving Hex value: {0}", e.getMessage());
        }
        return ret;
	}
	
	public void setFieldToByte(IFieldDetail field, byte val) {
		
	    ensureCapacity(field.calculateActualEnd(this));

        
        int en = field.calculateActualEnd(this);
        for (int i = field.calculateActualPosition(this) - 1; i < en; i++) {
        	data[i] = val;
        }
        super.checkForOdUpdate(field);
	}

	/**
	 * @see java.lang.Object#clone()
	 */
	public Object clone() {
		return lineProvider.getLine(layout, data.clone());
	}
	
	
	@Override
	public boolean isDefined(IFieldDetail field) {
		if (this.data == null || data.length <= field.getPos() || ! super.isFieldInLine(field)) {
			return false;
		}
		boolean ret = false;
		Type t = TypeManager.getInstance().getType(field.getType());
		if (t instanceof TypeNum) {
			ret = ((TypeNum) t).isDefined(this, data, field);
		} else {
			int pos = field.calculateActualPosition(this);
			ret = ! TypeChar.isHexZero(data, pos, field.getLen());
		}
		return ret;
	}




	@Override
	public final FieldValueLine getFieldValue(IFieldDetail field) {
		return new FieldValueLine(this, field);
	}

	@Override
	public final FieldValueLine getFieldValue(int recordIdx, int fieldIdx) {
		return new FieldValueLine(this, recordIdx, fieldIdx);
	}
}