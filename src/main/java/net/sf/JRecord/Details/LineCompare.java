/*
 * @Author Bruce Martin
 * Created on 23/01/2007
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

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.Date;

import net.sf.JRecord.Types.Type;

/**
 * This class will compare two lines
 *
 * @author Bruce Martin
 *
 */
public class LineCompare implements Comparator<AbstractLine> {

    //private LayoutDetail layout;
    private RecordDetail recordDetail = null;
    private final int recordIdx;
    private final int[] fields2compare;
    private final boolean[] descending;
    private final int[] fieldType;

    private int numFields2compare = 0;


    /**
     * This class will compare two lines
     *
     * @param layoutDetails layout of the lines
     * @param recordId record index (or identifier)
     * @param fields fields to be compared
     * @param descendingSequence is it ascending sequence
     */
    public LineCompare(final LayoutDetail layoutDetails,
            final int recordId,
            final int[] fields,
            final boolean[] descendingSequence) {
        super();

        //layout = layoutDetails;
        descending = descendingSequence;
        fields2compare = fields;

        if (recordId >= layoutDetails.getRecordCount()) {
            recordIdx = -1;
            fieldType = null;
        } else {
            recordDetail = layoutDetails.getRecord(recordId);
            recordIdx = recordId;

            numFields2compare = Math.min(fields.length, descending.length);
            //recordDetail.getFieldsNumericType(1);

            fieldType = new int[numFields2compare];
            for (int i = 0; i < numFields2compare; i++) {
                fieldType[i] = recordDetail.getFieldsNumericType(fields[i]);
            }
        }
    }

    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(AbstractLine o1, AbstractLine o2) {
        AbstractLine l1 = o1;
        AbstractLine l2 = o2;
        int   i = 0;
        int ret = 0;
        int idx;

        if (recordDetail == null) {
            return l1.getFullLine().compareTo(l2.getFullLine());
        }
        while (ret == 0 && i < this.numFields2compare) {
            idx = fields2compare[i];
            if (fieldType[i] == Type.NT_DATE) {
                @SuppressWarnings("deprecation")
				Date d1 = toDate(l1.getField(this.recordIdx, idx));
                @SuppressWarnings("deprecation")
				Date d2 = toDate(l2.getField(this.recordIdx, idx));
                if (d1 == null) {
                    ret = -1;
                    if (d2 == null) {
                        ret = 0;
                    }
                } else if (d2 == null) {
                    ret = 1;
                } else {
                    try {
                        ret = d1.compareTo(d2);
                    } catch (Exception e) {
                        ret = 0;
                    }
                }
            } else if (l1 == null || l2 == null) {
            	ret = 1;
            	if (l1 == null && l2 == null) {
            		ret = 0;
            	} else if (l1 == null) { 
            		ret = -1;
            	}
            } else {
                @SuppressWarnings("deprecation")
				String s1 = toString(l1.getField(this.recordIdx, idx));
                @SuppressWarnings("deprecation")
				String s2 = toString(l2.getField(this.recordIdx, idx));

                if (fieldType[i] != Type.NT_TEXT) {
                    try {
                        ret = (new BigDecimal(s1)).compareTo((new BigDecimal(s2)));
                    } catch (Exception e) {
                    }
                } else {
                    ret = s1.compareTo(s2);
                }
            }
            if (this.descending[i]) {
                ret = 0 - ret;
            }
            i += 1;
        }

        return ret;
    }
    
    
    /**
     * convert object to string
     * @param o object to be converted
     * @return string equivalent
     */
    private String toString(Object o) {
    	String s = "";
    	if (o != null) {
    		s = o.toString();
    	}
    	return s;
    }

    /**
     * convert object to Date
     * @param o object to convert
     * @return as date
     */
    private Date toDate(Object o) {
        Date ret = null;
        try {
            ret = (Date) o;
        } catch (Exception e) {
        }
        return ret;
    }
}
