/*
 * @Author Bruce Martin
 * Created on 20/04/2007
 *
 * Purpose:
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
public class XmlLine extends ListLine {

    

	//private static LineProvider defaultProvider = new DefaultLineProvider();

	//private LayoutDetail layout;

	private boolean useField4Index = true;

	public XmlLine(LayoutDetail layoutDetails, int recordIdx) {
		super(layoutDetails);
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
	 * @see net.sf.JRecord.Details.AbstractLine#getFullLine()
	 */
	public String getFullLine() {
	    return "";
	}



    protected int getFieldNumber(int recordIdx, int fieldIdx) {
       	int idx = fieldIdx;

        //   	System.out.print("getField " + recordIdx + " " + fieldIdx);
           	if (useField4Index && recordIdx < layout.getRecordCount()
           	&& fieldIdx < layout.getRecord(recordIdx).getFieldCount()
       	&& fieldIdx >= 0
           	&& layout.getRecord(recordIdx).getField(fieldIdx).getPos() >= 0) {
           		idx = layout.getRecord(recordIdx).getField(fieldIdx).getPos();
           	}
           	return idx;
    }

   /**
     * Test if Tree rebuild is required
     */
	public boolean isRebuildTreeRequired() {
		boolean ret = rebuildRequired;
		return ret;
	}


	/**
	 * @param useField4Index the useField4Index to set
	 */
	public final void setUseField4Index(boolean useField4Index) {
		this.useField4Index = useField4Index;
	}
	

	@Override
	protected final int getAdj() {
		return 0;
	}

}
