package net.sf.JRecord.cg.schema.classDefinitions;

public class ClassDefYYMD extends ClassDef {

	private static final String[] CONVERSION_IMPORTS = {
			"java.time.LocalDate"
	};
	
	private final String adjp, adjm;
	public ClassDefYYMD() {
		this(null);
	};
	
	public ClassDefYYMD(String adj) {
		super("java.time.LocalDate", "LocalDate", "Int", CONVERSION_IMPORTS, null,
			  "\tprivate static LocalDate yymdToDate(int yymd) {return LocalDate.of(yymd / 10000, (yymd / 100) % 100, yymd % 100);}\n"
			+ "\tprivate static int dateToYYMD(LocalDate d) { return d.getYear() * 10000 + d.getMonthValue() * 100 + d.getDayOfMonth();}\n");

		if (adj == null) {
			adjp = "";
			adjm = "";
		} else {
			adjp = " + " + adj;
			adjm = " - " + adj;
		}
	}
	
	

	/**
	 * @see net.sf.JRecord.cg.schema.classDefinitions.ClassDef#generateToPojo(java.lang.String)
	 */
	@Override
	public String generateToPojo(String variable) {
		return "yymdToDate(" + variable + adjp + ")" ;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.schema.IClassDef#generateFromPojo(java.lang.String)
	 */
	@Override
	public String generateFromPojo(String variable) {
		return "dateToYYMD(" + variable + ")" + adjm;
	}

}
