package net.sf.JRecord.Details;

import java.util.Iterator;

import net.sf.JRecord.Common.AbstractFieldValue;


/**
 * Class to iterate over all fields for a Record
 *
 * @author Bruce Martin
 *
 */
public class FieldIterator implements Iterable<AbstractFieldValue>, Iterator<AbstractFieldValue> {

	private final AbstractLine line;
	private int fieldNo = 0;
	private final int recordNo;
	private final RecordDetail recordDef;


	public FieldIterator(AbstractLine line, int recordNum) {
		super();
		this.line = line;
		this.recordNo = recordNum;

		this.recordDef = line.getLayout().getRecord(recordNum);
	}

	/**
	 * Return the field values
	 * @param fieldNumber field number
	 * @return field values
	 */
	public AbstractFieldValue getFieldValue(int fieldNumber) {
		return new FieldValue(line, recordNo, fieldNumber);
	}

	/**
	 * Get the number of fields
	 * @return number of fields
	 */
	public int getFieldCount() {
		return recordDef.getFieldCount();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	@Override
	public boolean hasNext() {
		return fieldNo < recordDef.getFieldCount();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	@Override
	public AbstractFieldValue next() {
		return new FieldValue(line, recordNo, fieldNo++);
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	@Override
	public void remove() {
		throw new RuntimeException("Remove is not supported");
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<AbstractFieldValue> iterator() {
		return new FieldIterator(line, recordNo);
	}



}
