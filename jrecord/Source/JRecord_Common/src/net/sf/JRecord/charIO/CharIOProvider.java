package net.sf.JRecord.charIO;

import net.sf.JRecord.Common.Constants;

public class CharIOProvider {

	public ICharWriter getWriter(int id, String font, String eol, int length) {
		ICharWriter ret = null;
		switch (id) {
		case Constants.IO_FIXED_LENGTH_CHAR: ret = new FixedLengthCharWriter(length, font);		break;
		case Constants.IO_UNICODE_TEXT:
		case Constants.IO_CSV_NAME_1ST_LINE:
		case Constants.IO_CSV: 				 ret = new StandardCharWriter(eol, font);			break;
		}
		
		return ret;
	}
}
