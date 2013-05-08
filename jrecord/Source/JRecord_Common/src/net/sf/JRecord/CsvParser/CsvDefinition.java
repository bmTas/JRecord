/**
 *
 */
package net.sf.JRecord.CsvParser;

/**
 * @author mum
 *
 */
public class CsvDefinition implements ICsvDefinition {

	private final String delimiter, quote;
	private final int delimiterOrganisation, numberOfFields;



	public CsvDefinition(String delimiter, String quote) {
		this(delimiter, quote, ICsvDefinition.NORMAL_SPLIT, -1);
	}


	public CsvDefinition(String delimiter, String quote,
			int delimiterOrganisation) {
		this(delimiter, quote, delimiterOrganisation, -1);
	}


	public CsvDefinition(String delimiter, String quote,
			int delimiterOrganisation, int numberOfFields) {
		super();
		this.delimiter = delimiter;
		this.quote = quote;
		this.delimiterOrganisation = delimiterOrganisation;
		this.numberOfFields = numberOfFields;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ILineDetails#getDelimiter()
	 */
	@Override
	public String getDelimiter() {
		return delimiter;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#getQuote()
	 */
	@Override
	public String getQuote() {
		return quote;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ILineDetails#getDelimiterOrganisation()
	 */
	@Override
	public int getDelimiterOrganisation() {
		return delimiterOrganisation;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#getFieldCount()
	 */
	@Override
	public int getFieldCount() {
		return numberOfFields;
	}
}
