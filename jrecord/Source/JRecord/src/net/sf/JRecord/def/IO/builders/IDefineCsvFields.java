package net.sf.JRecord.def.IO.builders;


/**
 * <p>Interface for defining Csv field to a IOBuilder</p>
 * <pre>
 * Usage:
 * 
 *     ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV
 *             .newIOBuilder(";", "\"")
 *                     .defineFields()
 *                         .<b>addCsvField</b>(FLD_SKU,   Type.ftChar, 0)
 *                         .<b>addCsvField</b>(FLD_STORE, Type.ftNumAnyDecimal, 0)
 *                         .<b>addCsvField</b>(FLD_DATE,  Type.ftNumAnyDecimal, 0)
 *                         .<b>addCsvField</b>(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
 *                         .<b>addCsvField</b>(FLD_QTY,   Type.ftNumAnyDecimal, 0)
 *                         .<b>addCsvField</b>(FLD_PRICE, Type.ftNumAnyDecimal, 0)
 *                         .<b>addCsvField</b>(FLD_GST,   Type.ftNumAnyDecimal, 0)
 *                     .<b>endOfRecord</b>();
 *   </pre>
 * 
 * @author Bruce Martin
 *
 */
public interface IDefineCsvFields {
	/**
	 * Add a Csv field to the schema
	 * @param name field name
	 * @param type Field type, Most commonly it will be <b>Type.ftChar</b>; but you could use
	 * <b>Type.ftNumAnyDecimal</b> for a number or <b>Type.ftNumLeftJustified</b> for a type.
	 * 
	 * @param decimal how many decimals (for fixed length numeric types). Normally you should leave it as
	 *  zero.
	 * @return this Schema-builder so other fields can be added
	 * 
	 * <p><b>Example:</b>
	 * <pre>
	 *             .defineFields()
     *                 .<b>addCsvField</b>(FLD_SKU,   Type.ftChar, 0)
     *                 .<b>addCsvField</b>(FLD_STORE, Type.ftNumAnyDecimal, 0)
     *             .endOfRecord();
     * </pre>                    
	 */
	public IDefineCsvFields addCsvField(String name, int type, int decimal);

	/**
	 * Marks the end of Field (or Column) Definition and returns the CsvIOBuilder
	 * 
	 * @return CsvIOBuilder for further
	 */
	public ICsvIOBuilder endOfRecord();
}
