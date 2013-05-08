/*
 * @Author Bruce Martin
 * Created on 16/03/2007
 *
 * Purpose:
 * Abstract record that can be editted by the user and keeps track
 * of wether it is new / has been updated. This is important if the
 * record has come from a DB.
 */
package net.sf.JRecord.External.Def;

/**
 * Abstract record that can be editted by the user and keeps track
 * of wether it is new Record / updated Record.
 * It is important to keep track of wether the record has been update for
 * DB updates.
 *
 * This class forms basis of all parts of a Record Description that are written
 * to external files / DB (i.e. ExternalRecord / ExternalField + DBRecord / DBField in
 * the RecordEditor)..
 *
 *
 * @author Bruce Martin
 *
 */
public class AbstractUpdatableRecord {

    public static final int UNCHANGED  = 1;
	//final public static int Inserted  = 2;
    public static final int NEW_RECORD = 2;
    public static final int UPDATED    = 3;
    public static final int BLANK_RECORD = 4;

    public static final int NULL_INT_VALUE  = -1;

	protected int updateStatus = UNCHANGED;

	public static final byte[]   NULL_BYTES = {};

	private boolean newRecord = false;

	/**
	 * Create a new record
	 *
	 * @param isNull wether it is a Null record
	 */
	public AbstractUpdatableRecord(final boolean isNull) {
		super();

		if (isNull) {
			updateStatus = NULL_INT_VALUE;
			newRecord = true;
		}
	}

	/**
	 * Get the update Status of the record
	 *
	 * @return update Status
	 */
	public int getUpdateStatus() {
		return updateStatus;
	}

	/**
	 * Set update Status
	 *
	 * @param status new update status
	 */
	public void setUpdateStatus(int status) {
		updateStatus = status;
	}


	/**
     * compares to byte arrays
     *
     * @param val1 value 1
     * @param val2 value 2
     *
     * @return wether val1 = val2
     */
    protected final boolean isEqual(byte[] val1, byte[] val2) {
        boolean more;
        int i;

        if (val1 == null && val2 == null) {
            return true;
        }

        more = val1 != null && val2 != null && val1.length == val2.length;

        for (i = 0; more && i < val1.length; i++) {
            more = val1[i] == val2[i];
        }

        return more;
    }

	/**
	 * wether this is a new record or not.
	 * @return Returns the inserted.
	 */
	public boolean isNew() {
		return newRecord;
	}


	/**
	 * Set the "isNew" status of the record.
	 * @param pInserted The "new" status to set.
	 */
	public void setNew(boolean pInserted) {
		this.newRecord = pInserted;
	}

	public final boolean equals(String val, String fieldValue) {

		if ((val == null || "".equals(val))
		&& (fieldValue == null || "".equals(fieldValue))) {

		} else if ((val == null) || (! val.equals(fieldValue)) || (updateStatus == NULL_INT_VALUE)) {
			updateStatus = UPDATED;
			return false;
		}
		return true;
	}
}
