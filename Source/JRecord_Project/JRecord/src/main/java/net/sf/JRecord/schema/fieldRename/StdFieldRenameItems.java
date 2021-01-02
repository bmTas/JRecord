package net.sf.JRecord.schema.fieldRename;

import net.sf.JRecord.fieldNameConversion.IRenameField;
import net.sf.JRecord.fieldNameConversion.NoChangeConversion;

public class StdFieldRenameItems {
	/**
	 * These values allow you to "Rename the Cobol Names"
	 */
	public static final int RO_LEAVE_ASIS = 0;
	public static final int RO_MINUS_TO_UNDERSCORE = 1;
	public static final int RO_CAMEL_CASE = 2;

	public static final IRenameField CAMEL_CASE = new IRenameField() {
		@Override public String toFieldName(String name) {
			char c;
			int l = name.length();
			boolean toUpper = false;
			String lcName = name.toLowerCase();
			String ucName = name.toUpperCase();
			StringBuilder b = new StringBuilder(name.length()) ;
			for (int i = 0; i < l; i++) {
				c = name.charAt(i);
				switch (c) {
				case '-':
				case ' ':
				case '_':
					toUpper = true;
					break;
				default:
					if (toUpper) {
						c = ucName.charAt(i);
					} else {
						c = lcName.charAt(i);
					}
					b.append(c);
					
					toUpper = false;
				}
			}
			return b.toString();
		}
	};
	
	public static final IRenameField MINUS_TO_UNDERSCORE = new IRenameField() {
		@Override public String toFieldName(String name) {
			int l = name.length();
			StringBuilder b = new StringBuilder(name) ;
			for (int i = 0; i < l; i++) {
				switch (name.charAt(i)) {
				case '-':
				case ' ':
					b.setCharAt(i, '_');
					break;
				}
			}
			return b.toString();		
		}
	};
	
	
	public static final IRenameField LEAVE_ASIS = new IRenameField() {
		@Override public String toFieldName(String name) {
			return name;
		}
	};
	
	public static IRenameField replaceMinusWith(String sep) {
		return new NoChangeConversion(sep, ""); 
	}

	public static IRenameField getRenameField(int opt) {
		IRenameField ret =  LEAVE_ASIS;
	
		switch (opt) {
		case RO_CAMEL_CASE:				ret = CAMEL_CASE;			break;
		case RO_MINUS_TO_UNDERSCORE:	ret = MINUS_TO_UNDERSCORE;
		}
		return ret;
	}
}
