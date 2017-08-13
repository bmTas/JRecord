package net.sf.JRecord.CsvParser;

/**
 * 
 * @author Bruce Martin
 *
 * @deprecated Use CsvParserManagerChar
 */
public class ParserManager extends CsvParserManagerChar {

	private static ParserManager instance = new ParserManager();

	/**
	 * 
	 * @return  parser manager
	 * 
	 * @deprecated   use  @deprecated Use <b>CsvParserManagerChar.getInstance();</b>
	 */
	public static ParserManager getInstance() {
		return instance;
	}

}
