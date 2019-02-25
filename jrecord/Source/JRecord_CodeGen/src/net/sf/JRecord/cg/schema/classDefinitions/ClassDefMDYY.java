package net.sf.JRecord.cg.schema.classDefinitions;

public class ClassDefMDYY extends ClassDef {

	private static final String[] CONVERSION_IMPORTS = {
			"java.time.LocalDate"
	};
	public ClassDefMDYY() {
		super("java.time.LocalDate", "LocalDate", "Int", CONVERSION_IMPORTS, null,
			  "\tprivate static LocalDate mdyyToDate(int mdyy) {return LocalDate.of(mdyy % 10000, mdyy / 1000000, (mdyy / 10000) % 100);}\n"
			+ "\tprivate static int dateToMDYY(LocalDate d) { return d.getYear()  + d.getMonthValue() * 1000000 + d.getDayOfMonth() * 10000;}\n");
	}

	/**
	 * @see net.sf.JRecord.cg.schema.classDefinitions.ClassDef#generateToPojo(java.lang.String)
	 */
	@Override
	public String generateToPojo(String variable) {
		return "mdyyToDate(" + variable + ")" ;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.schema.IClassDef#generateFromPojo(java.lang.String)
	 */
	@Override
	public String generateFromPojo(String variable) {
		return "dateToMDYY(" + variable + ")" ;
	}
}
