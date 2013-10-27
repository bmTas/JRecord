/*
 * @Author Bruce Martin
 * Created on 20/04/2007
 *
 * Purpose:
 */
package net.sf.JRecord.Details;

import java.util.ArrayList;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Common.XmlConstants;

/**
 * Line for use with XML files.
 *
 * <p>The one important method is getFieldValue
 *
 * <p>Creating:
 * <pre>
 *              AbstractLine outLine = <font color="brown"><b>new</b></font> XmlLine(oLayout, recordIdx);
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
 *
 */
public class XmlLine extends BaseLine {

    private ArrayList<Object> fields = new ArrayList<Object>();

	//private static LineProvider defaultProvider = new DefaultLineProvider();

	//private LayoutDetail layout;

	private int preferredLayout = Constants.NULL_INTEGER;
	private boolean newRecord;
	private boolean rebuildRequired = false;
	private boolean useField4Index = true;

	public XmlLine(LayoutDetail layoutDetails, int recordIdx) {
	    layout = layoutDetails;
	    newRecord = recordIdx < 0;
	    preferredLayout = recordIdx;

	    fields.add("");

	    if (newRecord) {
	    	fields.add("True");
	    }
	}


    /**
     * @see java.lang.Object#clone()
     */
    public Object clone() {
        Object ret = null;

        try { ret = super.clone(); } catch (Exception e) {}

        if (! (ret instanceof XmlLine)) {
        	XmlLine line = new XmlLine(layout, preferredLayout);
        	for (int i = 0; i < fields.size(); i++) {
//        		Object o = fields.get(i);
//        		if (o != null
//        		&& ! (o instanceof String
//        		   || o instanceof Boolean
//        		   || o instanceof BigInteger
//        		   || o instanceof BigDecimal)) {
//					   o = o.toString();
//				}
        		try {
        			line.setRawField(preferredLayout, i, fields.get(i));
        		} catch (Exception e) {
				}
        	}
        	ret = line;
        }

        return ret;
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
     * @see net.sf.JRecord.Details.AbstractLine#getField(net.sf.JRecord.Common.FieldDetail)
     */
    public Object getField(IFieldDetail field) {
        return getFieldRaw(preferredLayout, field.getPos());
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
     * @see net.sf.JRecord.Details.AbstractLine#getFullLine()
     */
    public String getFullLine() {
        return "";
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
    }

    /**
     * @see net.sf.JRecord.Details.AbstractLine#setData(java.lang.String)
     */
    public void setData(String newVal) {
        // TODO Auto-generated method stub
    }

    /**
     * @see net.sf.JRecord.Details.AbstractLine#setField(net.sf.JRecord.Common.FieldDetail, java.lang.Object)
     */
    public void setField(IFieldDetail field, Object value)
            throws RecordException {
    	if (field == null && (value == null || "".equals(value.toString()))) return;
    	setRawField(preferredLayout, field.getPos(), value);
    }

     /**
     * @see net.sf.JRecord.Details.AbstractLine#setField(int, int, java.lang.Object)
     */
    public void setField(int recordIdx, int fieldIdx, Object val)
    throws RecordException {
    	setRawField(recordIdx, getFieldNumber(recordIdx, fieldIdx), val);
    }

    private int getFieldNumber(int recordIdx, int fieldIdx) {
       	int idx = fieldIdx;

        //   	System.out.print("getField " + recordIdx + " " + fieldIdx);
           	if (useField4Index && recordIdx < layout.getRecordCount()
           	&& fieldIdx < layout.getRecord(recordIdx).getFieldCount()
           	&& layout.getRecord(recordIdx).getField(fieldIdx).getPos() >= 0) {
           		idx = layout.getRecord(recordIdx).getField(fieldIdx).getPos();
           	}
           	return idx;
    }

   public void setRawField(int recordIdx, int fieldIdx, Object val)
            throws RecordException {

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
    public void setField(String fieldName, Object value) throws RecordException {
        setField(getFieldFromName(fieldName), value);
    }

    /**
     * @see net.sf.JRecord.Details.AbstractLine#setFieldHex(int, int, java.lang.String)
     */
    public String setFieldHex(int recordIdx, int fieldIdx, String val)
            throws RecordException {
        // TODO Auto-generated method stub
        return ""; //super.setFieldHex(recordIdx, fieldIdx, val);
    }

    /**
     * @see net.sf.JRecord.Details.AbstractLine#setFieldText(int, int, java.lang.String)
     */
    public void setFieldText(int recordIdx, int fieldIdx, String value)
            throws RecordException {
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

    /**
     * Test if Tree rebuild is required
     */
	public boolean isRebuildTreeRequired() {
		boolean ret = rebuildRequired;
		rebuildRequired = false;
		return ret;
	}


	/**
	 * @param useField4Index the useField4Index to set
	 */
	public final void setUseField4Index(boolean useField4Index) {
		this.useField4Index = useField4Index;
	}

	protected final FieldDetail getFieldFromName(String fieldName) {
		FieldDetail fldDef = null;
		if (preferredLayout >= 0) {
			fldDef = layout.getRecord(preferredLayout).getField(fieldName);
		}
		return fldDef;
	}
}
