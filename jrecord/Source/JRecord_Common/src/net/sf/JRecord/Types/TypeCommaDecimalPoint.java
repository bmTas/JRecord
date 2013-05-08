package net.sf.JRecord.Types;

import java.text.NumberFormat;
import java.util.Locale;

import net.sf.JRecord.Common.Conversion;

public class TypeCommaDecimalPoint extends TypeNum {



	private static final NumberFormat GERMAN_NUM_FORMAT = NumberFormat.getIntegerInstance(Locale.GERMAN);

	public TypeCommaDecimalPoint(int typeId, boolean isPositive) {
		super(typeId, isPositive);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#getNumberFormat()
	 */
	@Override
	protected NumberFormat getNumberFormat() {
		return GERMAN_NUM_FORMAT;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#addDecimalPoint(java.lang.String, int)
	 */
	@Override
	protected String addDecimalPoint(String s, int decimal) {
		//if (decimal > 0 && s.length() > decimal)  {
			s = s.trim();
			int pos = s.lastIndexOf(',');

			if (pos >= 0) {
				s =  s.substring(0, pos) + '.' + s.substring(pos + 1);
			}
		//}

		return Conversion.numTrim(s);
	}

//	/**
//	 * for Testing
//	 * @param s String to format
//	 * @param decimal number of decimal places
//	 * @return
//	 */
//	public final String addDP(String s, int decimal) {
//		return addDecimalPoint(s, decimal);
//	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeChar#getDecimalChar()
	 */
	@Override
	public char getDecimalChar() {
		return ',';
	}


}
