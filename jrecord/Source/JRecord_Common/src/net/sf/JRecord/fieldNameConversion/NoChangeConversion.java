package net.sf.JRecord.fieldNameConversion;

import net.sf.JRecord.Common.Conversion;

public class NoChangeConversion implements IFieldNameConversion {

	static final NoChangeConversion ASIS = new NoChangeConversion("_", "Replace - with 1 underscores (_)");
	static final NoChangeConversion DOUBLE_UNDERSCORE = new NoChangeConversion("__", "Replace - with 2 underscores (_)");

	
	private final String sepString, name;
	
	public NoChangeConversion(String sepString, String name) {
		this.sepString = sepString;
		this.name = name;
	} 

	@Override
	public String getConversionName() {
		return name;
	}

	public String getConversionDescription() {
		return "This option replaces - with " + sepString + "\n\n"
				+ "i.e. DTAR020_KEYCODE_NUMBER  --->   DTAR020"
				+ sepString + "KEYCODE" + sepString + "NUMBER";
	} 
	
	
	@Override
	public String toAdjCobolName(String cobolName, String copybookName) {
		return convertToPgmId(cobolName, true).toString();
//		if (cobolName==null) {
//			return null;
//		}
//		return Conversion.replace(cobolName, "-", sepString).toString();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toConstant(java.lang.StringBuilder)
	 */
	@Override
	public String toConstant(String b) {
		b = Conversion.replace(convertToPgmId(b, true), "-", sepString).toString();
		return b.toUpperCase();
    }
    
    

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toSqlName(java.lang.String)
	 */
	@Override
	public String toSqlName(String str) {
		StringBuilder b = convertToPgmId(str.toLowerCase(), true);
//		b = Conversion.replace(new StringBuilder(b), "-", "_");
		boolean toUpper = true;
		for (int i = 0; i < b.length(); i++) {
			char ch = b.charAt(i);
			switch (ch) {
			case '-' :
			case '_' :
				b.setCharAt(i, '_');
				toUpper = true;
				break;
			default:
				if (toUpper) {
					b.setCharAt(i, Character.toUpperCase(ch));
					toUpper = false;
				} 
			}
		}
		return b.toString();
    }
    

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toSuffix(java.lang.String)
	 */
	@Override
	public String toSuffix(String str) {
		return convertToPgmId(str, true).toString();
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toFieldName(java.lang.String)
	 */
	@Override
	public String toFieldName(String b) {
		return toJavaId(b, 'f');
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toClassName(java.lang.String)
	 */
	@Override
	public String toClassName(String b) {
		return toJavaId(b, 'c');
	}

	
	private String toJavaId(String str, char pref) {
		StringBuilder b = convertToPgmId(str, true);
		if (b == null || b.length() == 0) {
			return "";
		}
		
		if ((b.charAt(0) >= 'A' && b.charAt(0) <= 'Z')) {
			//b.setCharAt(0, Character.toLowerCase(b.charAt(0)));
		} else if (b.charAt(0) < 'a' || b.charAt(0) > 'z') {
			b.insert(0, pref);
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toJavaId(boolean, java.lang.String)
	 */
	@Override
	public String toJavaId(boolean isCobol, String name) {
		if (isCobol) {
			return cobolName2JavaName(name);
		}
		return string2JavaId(name);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#cobolName2JavaName(java.lang.String)
	 */
	@Override
	public String cobolName2JavaName(String cobolName) {
		return convertToPgmId(cobolName, false).toString();
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#string2JavaId(java.lang.String)
	 */
	@Override
	public String string2JavaId(String name) {
		return convertToPgmId(name, true).toString();
	}



	private StringBuilder convertToPgmId(String name, boolean keepSeperators) {
		int length = name.length();
		StringBuilder b = new StringBuilder(length);

		char c;
		
		for (int i = 0; i < length; i++) {
			c = name.charAt(i);
			switch (c) {
			case ':':
			case ';':
			case '*':
			case '=':
			case '+':
			case '\'':
			case '\"':
			case '~':
			case '!':
			case '|':
			case '@':
			case '#':
			case '$':
			case '%':
			case ')':
			case '[':
			case ']':
				break;
			case '(':
			case ',':
			case '/':
			case '\\':
			case ' ':
			case '.':
				if (b.length() == 0 || (b.charAt(b.length() - 1) != '_')) {
					b.append('_');
				}
				break;
			case '_':
				b.append('_');
				break;
			case '-':
				b.append(sepString);
				break;
			default:
				b.append(c);
			}
		}
		return b;
	}
	

}
