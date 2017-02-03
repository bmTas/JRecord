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

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.XmlConstants;

public abstract class ListLine extends BaseLine {

	protected List<Object> fields = new ArrayList<Object>();
	protected int preferredLayout = Constants.NULL_INTEGER;
	protected boolean newRecord;
	protected boolean rebuildRequired = false;


	public ListLine(LayoutDetail layoutDetails) {
		super();
		
		layout = layoutDetails;
	    newRecord = false;
	    preferredLayout = 0;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getData()
	 */
	public byte[] getData() {
	    return null;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getData(int, int)
	 */
	public byte[] getData(int start, int len) {
	    return null;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getField(IFieldDetail)
	 */
	public Object getField(int typeId, IFieldDetail field) {
	    return getFieldRaw(preferredLayout, field.getPos() - getAdj());
	}
	
	

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getField(int, int)
	 */
	public Object getField(int recordIdx, int fieldIdx) {
    	int idx = getFieldNumber(recordIdx, fieldIdx);

        if (fields.size() > idx && idx >= 0) {
        	//if (newRecord) System.out.println("Get (New) : " + fieldIdx + " " + fields.get(fieldIdx));
//        	System.out.println(fields.get(idx));
            return fields.get(idx);
        } else if (fieldIdx == Constants.FULL_LINE) {
        	return getFullLine();
        }
//        System.out.println();
        //if (newRecord) System.out.println("Get (New) : " + fieldIdx + " null");
        return null;
    }

	private Object getFieldRaw(int recordIdx, int fieldIdx) {
	
	    if (fields.size() > fieldIdx && fieldIdx >= 0) {
	    	//if (newRecord) System.out.println("Get (New) : " + fieldIdx + " " + fields.get(fieldIdx));
	        return fields.get(fieldIdx);
	    }
	    //if (newRecord) System.out.println("Get (New) : " + fieldIdx + " null");
	    return null;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getField(java.lang.String)
	 */
	public Object getField(String fieldName) {
	    try {
	    	return getField(preferredLayout,
	    			layout.getRecord(preferredLayout).getFieldIndex(fieldName));
	    } catch (Exception e) {
			return null;
		}
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getFieldBytes(int, int)
	 */
	public byte[] getFieldBytes(int recordIdx, int fieldIdx) {
	    return null;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getFieldHex(int, int)
	 */
	public String getFieldHex(int recordIdx, int fieldIdx) {
	    return null;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getFieldText(int, int)
	 */
	public String getFieldText(int recordIdx, int fieldIdx) {
	    String s = "";
	    Object o;
	
	    if (fieldIdx < fields.size()
	    && (o = fields.get(fieldIdx)) != null) {
	        s = o.toString();
	    }
	    return s;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getPreferredLayoutIdx()
	 */
	@Override
	public int getPreferredLayoutIdx() {
	    return preferredLayout;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getPreferredLayoutIdx()
	 */
	@Override
	public int getPreferredLayoutIdxAlt() {
	    return preferredLayout;
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#replace(byte[], int, int)
	 */
	public void replace(byte[] rec, int start, int len) {
		throw new RuntimeException("replace not implemented for ListLine !!!");
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setData(java.lang.String)
	 */
	public void setData(String newVal) {
		throw new RuntimeException("setData not implemented for ListLine !!!");
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setField(IFieldDetail, Object)
	 */
	public void setField(IFieldDetail field, Object value) {
		if (field == null && (value == null || "".equals(value.toString()))) return;
		setRawField(preferredLayout, field.getPos() - getAdj(), value);
	}

	protected abstract int getAdj();
	
	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setField(int, int, java.lang.Object)
	 */
	public void setField(int recordIdx, int fieldIdx, Object val) {
		setRawField(recordIdx, getFieldNumber(recordIdx, fieldIdx), val);
	}
	
	protected int getFieldNumber(int recordIdx, int fieldIdx) {
		return fieldIdx;
	}

	public void setRawField(int recordIdx, int fieldIdx, Object val)
			{

    	int endFieldNum = XmlConstants.END_INDEX; //layout.getRecordIndex(XmlConstants.END_ELEMENT);
        for (int i = fields.size(); i <= fieldIdx; i++) {
            fields.add(null);
        }

        if (val != null || fields.get(fieldIdx) != null) {
        	rebuildRequired = (fieldIdx == 0) || (fieldIdx == endFieldNum) || newRecord;
//        	if (newRecord) {
//    			preferredLayout = fieldIdx;
//    			fields.set(XmlConstants.NAME_INDEX, layout.getRecord(recordIdx).getRecordName());
//    			fields.set(endFieldNum, "True");
//    			newRecord = false;
//    			rebuildRequired = true;
//    		}

        	fields.set(fieldIdx, val);

        	if (val != null && fieldIdx == XmlConstants.NAME_INDEX) {
        		int idx = layout.getRecordIndex(val.toString());
        		if (idx >= 0) {
        			preferredLayout = idx;
        		}
        	}
        }
    }

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setField(java.lang.String, java.lang.Object)
	 */
	public void setField(String fieldName, Object value) {
	    setField(getFieldFromName(fieldName), value);
	}


	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setFieldHex(int, int, java.lang.String)
	 */
	public String setFieldHex(int recordIdx, int fieldIdx, String val) 	{
	    return ""; //super.setFieldHex(recordIdx, fieldIdx, val);
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setFieldText(int, int, java.lang.String)
	 */
	public void setFieldText(int recordIdx, int fieldIdx, String value) {
		setField(recordIdx, fieldIdx, value);
	}

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#setWriteLayout(int)
	 */
	public void setWriteLayout(int pWriteLayout) {
		preferredLayout = pWriteLayout;
		fields.set(XmlConstants.NAME_INDEX, layout.getRecord(preferredLayout).getRecordName());
	}

	/**
	 * @param pLayout The layouts to set.
	 */
	public void setLayout(final LayoutDetail pLayout) {
	//		System.out.print("set layout >> " + fields.get(0)
	//				+ " " + layout.getRecord(preferredLayout).getRecordName()
	//				+ " " + preferredLayout);
			preferredLayout = pLayout.getRecordIndex(layout.getRecord(preferredLayout).getRecordName());
	//		System.out.println(" " + preferredLayout + " --> " + pLayout.getRecord(preferredLayout).getRecordName());
			this.layout = pLayout;
		}

	/**
	 * Set the line provider
	 *
	 * @param pLineProvider The lineProvider to set.
	 */
	public void setLineProvider(LineProvider pLineProvider) {
	}


	protected final FieldDetail getFieldFromName(String fieldName) {
		FieldDetail fldDef = null;
		if (preferredLayout >= 0) {
			fldDef = layout.getRecord(preferredLayout).getField(fieldName);
		}
		return fldDef;
	}
	
	@Override
	public boolean isDefined(IFieldDetail fld) {
		return isDefined(0, fld.getPos() - getAdj());
	}
	
	@Override
	public boolean isDefined(int rec, int pos) {
		return fields != null 
			&& pos < fields.size() 
			&& fields.get(pos) != null;
	}

	@Override
	public void setData(byte[] newVal) {
		setData(Conversion.toString(newVal, layout.getFontName()));
	}
}