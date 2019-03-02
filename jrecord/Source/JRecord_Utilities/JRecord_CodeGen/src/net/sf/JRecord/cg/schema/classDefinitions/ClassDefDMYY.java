package net.sf.JRecord.cg.schema.classDefinitions;

public class ClassDefDMYY extends ClassDef {
	private static final String[] CONVERSION_IMPORTS = {
			"java.time.LocalDate"
	};
	public ClassDefDMYY() {
		super("java.time.LocalDate", "LocalDate", "Int", CONVERSION_IMPORTS, null,
			  "\tprivate static LocalDate dmyyToDate(int dmyy) {return LocalDate.of(dmyy % 10000, (dmyy / 10000) % 100, dmyy / 1000000);}\n"
			+ "\tprivate static int dateToDMYY(LocalDate d) { return d.getDayOfMonth() * 1000000 + d.getMonthValue() * 10000 + d.getYear();}\n\n");
	}

	/**
	 * @see net.sf.JRecord.cg.schema.classDefinitions.ClassDef#generateToPojo(java.lang.String)
	 */
	@Override
	public String generateToPojo(String variable) {
		return "dmyyToDate(" + variable + ")" ;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.schema.IClassDef#generateFromPojo(java.lang.String)
	 */
	@Override
	public String generateFromPojo(String variable) {
		return "dateToDMYY(" + variable + ")";
				//variable + ".getDayOfMonth() * 100000 + " + variable + ".getMonthValue() * 100 + " + variable +".getYear()";
	}
}
