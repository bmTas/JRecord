/*
 * @Author Bruce Martin
 * Created on 10/09/2005
 *
 * Purpose: line to access DTAR0020 records
 */
package net.sf.JRecord.zExamples.userExtendedLine;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;

/**
 * Example of a hand coded <b>Line</b> to access fields in the DTAR0020
 * Cobol Copybook using getters and setters.
 *
 * @author Bruce Martin
 *
 */
public class LineDTAR0020 extends Line {

    private static int fieldIdx = 0;

    private static final int DTAR0020_RECORD = 0;

    private static final int KEYCODE_INDEX = fieldIdx++;
    private static final int STORE_INDEX   = fieldIdx++;
    private static final int DATE_INDEX    = fieldIdx++;
    @SuppressWarnings("unused")
	private static final int DEPT_INDEX    = fieldIdx++;
    private static final int QTY_INDEX     = fieldIdx++;
    private static final int SALES_INDEX   = fieldIdx++;


    /**
     * Create a null Line
     *
     * @param group Record Description
     */
    public LineDTAR0020(final LayoutDetail group) {
        super(group);
    }


    /**
     * Create Line from a byte record
     *
     * @param group Record Description
     * @param rec record
     */
    public LineDTAR0020(final LayoutDetail group, final byte[] rec) {
        super(group, rec);
    }


    /**
     * Create Line from a String record record
     *
     * @param group Record Description
     * @param rec   record
     */
    public LineDTAR0020(final LayoutDetail group, final String rec) {
        super(group, rec);
    }


    /**
     * Get the Keycode details
     *
     * @return keycode
     */
    public String getKeycode() {
        return super.getField(DTAR0020_RECORD, KEYCODE_INDEX).toString();
    }



    /**
     * Get the Store Number
     *
     * @return Store Number
     */
    public int getStore() {
        return Integer.parseInt(super.getField(DTAR0020_RECORD, STORE_INDEX).toString());
    }


    /**
     * Set the Store Number
     *
     * @param store Store Number
     *
     * @throws RecordException - conversion error
     */
    public void setStore(int store) throws RecordException {
        super.setField(DTAR0020_RECORD, STORE_INDEX, String.valueOf(store));
    }



    /**
     * Get the Date
     *
     * @return Date
     */
    public int getDate() {
        return Integer.parseInt(super.getField(DTAR0020_RECORD, DATE_INDEX).toString());
    }



    /**
     * Get the Quantity
     *
     * @return Quantity
     */
    public int getQuantity() {
        return Integer.parseInt(super.getField(DTAR0020_RECORD, QTY_INDEX).toString());
    }


    /**
     * Set the Quantity sold
     *
     * @param quantity Quantity
     *
     * @throws RecordException - conversion error
     */
    public void setQuantity(int quantity) throws RecordException {
        super.setField(DTAR0020_RECORD, QTY_INDEX, String.valueOf(quantity));
    }




    /**
     * Get the Quantity
     *
     * @return Quantity
     */
    public double getSalesRetail() {
        return Double.parseDouble(super.getField(DTAR0020_RECORD, SALES_INDEX).toString());
    }


    /**
     * Set the Quantity sold
     *
     * @param quantity Quantity
     *
     * @throws RecordException - conversion error
     */
    public void setSalesRetail(double quantity) throws RecordException {
        super.setField(DTAR0020_RECORD, SALES_INDEX, String.valueOf(quantity));
    }

}
