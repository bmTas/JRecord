package net.sf.JRecord.fieldNameConversion;

import net.sf.JRecord.Common.Conversion;

public class StdCblFieldNameConversion implements IFieldNameConversion {



	@Override
	public String getConversionName() {
		return "Standard Cobol to Java name conversion";
	}

	public String getConversionDescription() {
		return "This option does a standard Cobol Field Name to Java conversion\n\n"
				+ "i.e. DTAR020_KEYCODE_NUMBER  --->   keycodeNumber";
	}
	
	
	@Override
	public String toAdjCobolName(String cobolName, String copybookName) {
		String adjCobolName = cobolName==null? "" :cobolName;
		
		if (copybookName != null && copybookName.length() > 0) {
			String lcAdjCobolName = adjCobolName.toLowerCase();
			String lcCopybookName = copybookName.toLowerCase();
			if (lcAdjCobolName.length() > lcCopybookName.length() && lcAdjCobolName.startsWith(lcCopybookName)) {
				adjCobolName = adjCobolName.substring(copybookName.length());
				if (adjCobolName.startsWith("-") || adjCobolName.startsWith("_")) {
					adjCobolName = adjCobolName.substring(1);
				}
			} 
		}
		return convertToPgmId(adjCobolName, adjCobolName, true).toString();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toConstant(java.lang.StringBuilder)
	 */
	@Override
	public String toConstant(String b) {
		b = Conversion.replace(b, "-", "_").toString();
		return b.toUpperCase();
    } 
    
    

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#toSqlName(java.lang.String)
	 */
	@Override
	public String toSqlName(String str) {
		str = str.toLowerCase();
		StringBuilder b = convertToPgmId(str, str, true);
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
		StringBuilder b = convertToPgmId(str, str, true);
		if (b == null || b.length() == 0) {
			return "";
		}
		b.setCharAt(0, Character.toUpperCase(b.charAt(0)));
		return b.toString();
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
		StringBuilder b = convertToPgmId(str, str, true);
		if (b == null || b.length() == 0) {
			return "";
		}
		
		if ((b.charAt(0) >= 'A' && b.charAt(0) <= 'Z')) {
			b.setCharAt(0, Character.toLowerCase(b.charAt(0)));
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
		return convertToPgmId(cobolName.toLowerCase(), cobolName.toUpperCase(), false).toString();
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.nameConversion.INameConversion#string2JavaId(java.lang.String)
	 */
	@Override
	public String string2JavaId(String name) {
		return convertToPgmId(name, name.toUpperCase(), true).toString();
	}



	private StringBuilder convertToPgmId(String name, String ucCobolName, boolean keepSeperators) {
		int length = ucCobolName.length();
		StringBuilder b = new StringBuilder(length);

		boolean toUCase = false; 
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
				b.append('_');
				toUCase = false;
				break;
			case '/':
			case '\\':
			case '.':
			case ' ':
			case '-':
			case '_':
				if (keepSeperators) {
					b.append('_');
				}
				toUCase = true;
				break;
			default:
				if (toUCase) {
					b.append(ucCobolName.charAt(i));
					toUCase = false;
				} else {
					b.append(c);
				}
			}
		}
		return b;
	}
	

}
