package net.sf.JRecord.cg.schema.classDefinitions;

public class DateFormatStr {
	public final String dateFormat, name, codeDateFormat, sep;
	public final boolean hasYYYY, startsWithYY, hasSep;

	public DateFormatStr(String dateFormat, String name, String sep) {
		super();
		this.dateFormat = dateFormat;
		this.name = name;
		this.sep = sep;
		this.hasYYYY = dateFormat.indexOf("yyyy") >= 0;
		this.startsWithYY = dateFormat.startsWith("yy");
		this.hasSep = sep != null && sep.length() > 0;
		
		
		String cdf = dateFormat;
		if (sep != null && sep.length() > 0) {
			int length = dateFormat.length();
			StringBuilder b = new StringBuilder(length);
			int dCount = 0, mCount = 0;
			
			for (int i = 0; i < length; i++) {
				char charAt = dateFormat.charAt(i);
				switch (charAt) {
				case 'd':
					if (dCount++ == 0) {
						b.append(charAt);
					}
					
					break;
				case 'M':
					switch (mCount++) {
					case 0 : b.append(charAt);		break;
					case 2 : b.append("MM");		break;
					}
					
					break;
				default:
					b.append(charAt);
				}
			}
			cdf = b.toString();		
		} else if (dateFormat.startsWith("dd") || (dateFormat.startsWith("MM") && ! dateFormat.startsWith("MMM"))) {
			cdf = dateFormat.substring(1);
		}
		codeDateFormat = cdf;
	}	
}
