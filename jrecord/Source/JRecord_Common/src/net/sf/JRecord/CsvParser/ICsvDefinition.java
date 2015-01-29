package net.sf.JRecord.CsvParser;

public interface ICsvDefinition {

	public int NORMAL_SPLIT = 1;
	public int SEP_FOR_EVERY_FIELD = 2;			// Delimiter for every field
	public int SEP_FOR_EVERY_FIELD_PLUS_END = 3;// Delimiter for every field + one at the end


	/**
	 * Get the field Delimiter
	 * @return field Delimiter
	 */
	public String getDelimiter();

	/**
	 * Get the Quote char
	 * @return Quote char
	 */
	public String getQuote();

	/**
	 * Get how the delimiter are organised (Normal, SEP_FOR_EVERY_FIELD, SEP_FOR_EVERY_FIELD_PLUS_END )
	 * @return
	 */
	public int getDelimiterOrganisation();

	/**
	 * Get the number of fields
	 * @return number of fields
	 */
	public int getFieldCount();

	/**
	 * Get character set
	 * @return
	 */
	public String getFontName();

	/**
	 * Wether is is a single byte font being used
	 * @return Wether is is a single byte font being used
	 */
	public boolean isSingleByteFont();

	/**
	 * wether it has embedded newline characters
	 * @return the fields could have embedded new line characters
	 */
	public abstract boolean isEmbeddedNewLine();
}
