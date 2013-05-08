package net.sf.JRecord.CsvParser;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Class contains comon code for use in other CSV parser's
 *
 *
 * @author Bruce Martin
 *
 */
public abstract class BaseCsvParser  {


    private boolean quoteInColNames;

    public BaseCsvParser(boolean quoteInColumnNames) {
    	quoteInColNames = quoteInColumnNames;
    }

    /**
     * Wether Quote is to be used in column names
     * @return
     */
	public boolean isQuoteInColumnNames() {
		return quoteInColNames;
	}

	/**
	 * Convert a line of column names into a list of column names
	 *
	 * @param line line of column names
	 * @param lineDef Csv Definition
	 * @return list of column names
	 */
	public List<String> getColumnNames(String line, ICsvDefinition lineDef) {
		ArrayList<String> ret = new ArrayList<String>();

        StringTokenizer tok = new StringTokenizer(
                line, lineDef.getDelimiter(), false);
        String s;
        String quote = lineDef.getQuote();

        if (quoteInColNames && quote != null && ! quote.equals("")) {
	        while (tok.hasMoreElements()) {
	            s = tok.nextToken();
	            if (s.startsWith(quote)) {
	            	s = s.substring(1);
	            }
	            if (s.endsWith(quote)) {
	            	s = s.substring(0, s.length() - 1);
	            }
	            ret.add(s);
	        }
        } else {
	        while (tok.hasMoreElements()) {
	            ret.add(tok.nextToken());
	        }
        }

        return ret;
	}

	/**
	 * Convert a list of column names into a line
	 *
	 * @param names list of column names
	 * @param lineDef Csv Definition
	 * @return
	 */
	public String getColumnNameLine(List<String> names, ICsvDefinition lineDef) {
		StringBuilder buf = new StringBuilder();
		String currDelim = "";
		String quote = lineDef.getQuote();

		for (int i = 0; i < names.size(); i++) {
	           buf.append(currDelim)
               .append(quote)
               .append(names.get(i))
               .append(quote);
            currDelim = lineDef.getDelimiter();

		}

		return buf.toString();
	}
}
