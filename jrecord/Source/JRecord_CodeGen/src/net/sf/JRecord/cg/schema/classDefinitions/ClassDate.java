package net.sf.JRecord.cg.schema.classDefinitions;


public class ClassDate extends ClassDef {

	private static final String[] CONVERSION_IMPORTS = {
			"java.time.LocalDate",
			"java.time.format.DateTimeFormatter"
	};
	private static final String[] CONVERSION_IMPORTS_YY = {
			"java.time.LocalDate",
			"java.time.Year",
			"java.time.format.DateTimeFormatter",
			"java.time.format.DateTimeFormatterBuilder",
			"java.time.temporal.ChronoField"
	};
	
	
	private final String dateName;
	public ClassDate(DateFormatStr formatDef) {
		super("java.time.LocalDate", "LocalDate", "String", 
				formatDef.hasYYYY ?CONVERSION_IMPORTS : CONVERSION_IMPORTS_YY, 
				null,
				genCode(formatDef));
//			  "\tprivate static DateTimeFormatter formatter" + dateName + " =   DateTimeFormatter.ofPattern(\""+ dateDefStr +"\");\n"
//			 +"\tprivate static LocalDate toDate" + dateName + "(String date) {return LocalDate.parse(date, formatter" + dateName + ");}\n"
//			 +"\tprivate static String fromDate" + dateName + "(LocalDate date) {return formatter" + dateName + ".format(date);}\n\n");
		
		this.dateName = formatDef.name;
				
	}
	
	private static String genCode(DateFormatStr formatDef) {
		String dateName = formatDef.name;
		String codeDateFormat = formatDef.codeDateFormat;
		String ret = "\tprivate static DateTimeFormatter formatter" + dateName + " = ";
		int length = codeDateFormat.length();
		if (formatDef.hasYYYY) {
			ret +=  "  DateTimeFormatter.ofPattern(\""+ codeDateFormat +"\");\n";
		} else if (formatDef.startsWithYY) {
			ret +=	  " new DateTimeFormatterBuilder()\n"
					+ "\t\t.appendValueReduced(ChronoField.YEAR, 2, 2, Year.now().getValue() - 80)\n"
					+ "\t\t.appendPattern(\"" + codeDateFormat.substring(2) + "\")\n"
					+ "\t.toFormatter();\n";
		} else {
			ret +=	  " new DateTimeFormatterBuilder()\n"
					+ "\t\t.appendPattern(\"" + codeDateFormat.substring(0, length - 2) + "\")\n"
					+ "\t\t.appendValueReduced(ChronoField.YEAR, 2, 2, Year.now().getValue() - 80)\n"
					+ "\t.toFormatter();\n";
		}
		
		String source = "date";
		if ((! formatDef.hasSep) && (! (formatDef.hasYYYY && formatDef.startsWithYY))  && (! codeDateFormat.startsWith("MMM"))) {
			String zeroString = "0000000000000000000000000000000000000000".substring(0, length );
			source = "date.length() >= " + length + " ? date : \"" + zeroString + "\".substring(date.length()) + date ";
		}
		return ret 
		 + "\tprivate static LocalDate toDate" + dateName + "(String date) {return LocalDate.parse(" + source + ", formatter" + dateName + ");}\n"
		 + "\tprivate static String fromDate" + dateName + "(LocalDate date) {return formatter" + dateName + ".format(date);}\n\n";

	}

	/**
	 * @see net.sf.JRecord.cg.schema.classDefinitions.ClassDef#generateToPojo(java.lang.String)
	 */
	@Override
	public String generateToPojo(String variable) {
		return "toDate" + dateName + "(" + variable + ")" ;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.schema.IClassDef#generateFromPojo(java.lang.String)
	 */
	@Override
	public String generateFromPojo(String variable) {
		return "fromDate" + dateName + "(" + variable + ")" ;
	}
}
