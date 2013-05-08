package net.sf.JRecord.Types;

import java.math.BigDecimal;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

public class TypeNumAnyDecimal extends TypeNum {

	public TypeNumAnyDecimal(boolean isPositive) {
		super(true, false, true, isPositive, false);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#addDecimalPoint(java.lang.String, int)
	 */
	@Override
	protected String addDecimalPoint(String s, int decimal) {
		return s;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#formatValueForRecord(net.sf.JRecord.Common.FieldDetail, java.lang.String)
	 */
	@Override
	public String formatValueForRecord(IFieldDetail field, String val)
			throws RecordException {

        try {
            new BigDecimal(Conversion.numTrim(val));
        } catch (final Exception ex) {
            throw new RecordException("Invalid Number: {0}", ex.getMessage());
        }

	    if (isPositive() && val.indexOf('-') >= 0) {
	        throw new RecordException("Only positive numbers are allowed");
	    }

	    return val.trim();
	}
}
