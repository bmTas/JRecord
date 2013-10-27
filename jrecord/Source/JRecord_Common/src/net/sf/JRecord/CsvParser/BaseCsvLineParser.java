package net.sf.JRecord.CsvParser;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import net.sf.JRecord.Common.Constants;

/**
 * Class contains comon code for use in other CSV parser's
 *
 *
 * @author Bruce Martin
 *
 */
public abstract class BaseCsvLineParser  {


    private final boolean quoteInColNames;
	private final boolean allowReturnInFields;

    public BaseCsvLineParser(boolean quoteInColumnNames, boolean allowReturnInFields) {
    	quoteInColNames = quoteInColumnNames;
    	this.allowReturnInFields = allowReturnInFields;
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


	public int getFileStructure(ICsvDefinition csvDefinition, boolean namesOnFirstLine, boolean bin) {
		int ret = Constants.NULL_INTEGER;
		String quote = csvDefinition.getQuote();

		if ((csvDefinition.isEmbeddedNewLine() || allowReturnInFields)
		&& quote != null && ! "".equals(quote)) {
			ret = Constants.IO_CSV;
			if (bin) {
				ret = Constants.IO_BIN_CSV;
			} else if (! csvDefinition.isSingleByteFont()) {
				ret = Constants.IO_UNICODE_CSV;
			}

			if (namesOnFirstLine) {
				ret += 3;
			}
		}
		return ret;
	}
}
