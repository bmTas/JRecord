package net.sf.JRecord.cbl2csv.args;

public class FieldNameUpdaters {

	public static final IUpdateFieldName NO_UPDATE = new IUpdateFieldName() {		
		@Override public String updateName(String name) {
			return name;
		}
	};
	
	public static final IUpdateFieldName DROP_MINUS = new IUpdateFieldName() {		
		@Override public String updateName(String name) {
			StringBuilder b = new StringBuilder(name.length());
			for (int i = 0; i < name.length(); i++) {
				char ch = name.charAt(i);
				if (! (ch == '-' || ch == ' ')) {
					b.append(ch);
				}
			}
			return b.toString();
		}
	};
	
	public static final IUpdateFieldName TO_UNDERSCORE =  new IUpdateFieldName() {
		@Override public String updateName(String name) {
			StringBuilder b = new StringBuilder(name.length());
			for (int i = 0; i < name.length(); i++) {
				char ch = name.charAt(i);
				
				if (specialChar(ch)) {
					if (b.length() == 0 || b.charAt(b.length() - 1) != '_') {
						b.append('_');
					}
				} else {
					b.append(ch);
				}
			}
			if (b.length() > 1 && b.charAt(b.length()-1) == '_') {
				b.setLength(b.length()-1);
			}
			return b.toString();
		}		
	};
	
	public static final IUpdateFieldName TO_CAMEL_CASE =  new IUpdateFieldName() {
		@Override public String updateName(String name) {
			StringBuilder b = new StringBuilder(name.length());
			
			String lcName = name.toLowerCase();
			String ucName = name.toUpperCase();
			boolean ucase = false;
			for (int i = 0; i < name.length(); i++) {
				char ch = lcName.charAt(i);
				
				switch (ch)  {
				case ',':
					b.append('_');
					break;
				case '-':
				case '_':
				case '.':
				case ' ':
					ucase = b.length() > 0;
					break;
				default:
					if (ucase) {
						b.append(ucName.charAt(i));
						ucase = false;
					} else {
						b.append(ch);
					}
				}
			}			
			return b.toString();
		}		
	};
	
	public static final IUpdateFieldName TO_CAMEL_CASE_NO_ARRAY =  new IUpdateFieldName() {
		@Override public String updateName(String name) {
			StringBuilder b = new StringBuilder(name.length());
			
			String lcName = name.toLowerCase();
			String ucName = name.toUpperCase();
			boolean ucase = false;
			for (int i = 0; i < name.length(); i++) {
				char ch = lcName.charAt(i);
				
				if (ch == ',') {
					b.append('_');
				} else if (specialChar(ch)) {
					ucase =  b.length() > 0;
				} else if (ucase) {
					b.append(ucName.charAt(i));
					ucase = false;
				} else {
					b.append(ch);
				}
			}			
			return b.toString();
		}		
	};
	
	private static boolean specialChar(char ch) {
		boolean ret = false;
		switch (ch) {
		case '-':
		case ' ':
		case '(':
		case ')':
		case ',':
		case '.':
			ret = true;
			break;
		}
		return ret;
	}
	
	
}
