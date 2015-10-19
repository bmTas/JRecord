package net.sf.JRecord.Details;

import net.sf.JRecord.Common.IFieldDetail;

public class FieldValueLine extends FieldValue {

	private final Line theLine; 

	protected FieldValueLine(Line line, IFieldDetail fieldDetails) {
		super(line, fieldDetails);
		theLine = line;
	}

	protected FieldValueLine(Line line, int recordIndex, int fieldIndex) {
		super(line, line.getLayout().getField(recordIndex, fieldIndex));
		theLine = line;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isLowValues()
	 */
	@Override
	public boolean isLowValues() {
		return checkFor((byte) 0);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isHighValues()
	 */
	@Override
	public boolean isHighValues() {
		return checkFor((byte) -1);
	}
	
	private boolean checkFor(byte b) {
		byte[] bytes = theLine.getFieldBytes(field);
		
		if (bytes == null) {
			return b == 0;
		} 
		for (int i = 0; i < bytes.length; i++) {
			if (bytes[i] != b) {
				return false;
			}
		}
		
		return true;
	}

	/**
	 * @see net.sf.JRecord.Details.FieldValue#setHex(java.lang.String)
	 */
	@Override
	public void setHex(String val) {
		theLine.setFieldHex(field, val); 
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#setToLowValues()
	 */
	@Override
	public void setToLowValues() {
		theLine.setFieldToByte(field, (byte) 0);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#setToHighValues()
	 */
	@Override
	public void setToHighValues() {
		theLine.setFieldToByte(field, (byte) 0xFF);
	}



}
