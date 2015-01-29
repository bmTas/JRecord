package net.sf.JRecord.CsvParser;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Types.Type;

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

        if (quoteInColNames && quote != null && quote.length() > 0) {
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
		&& quote != null && quote.length() > 0) {
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

	/**
	 * Format field list as a Csv Line
	 * @param fields fields to be organised as a line
	 * @param lineDef Csv Line Definition
	 * @param fieldTypes Field types
	 * @return Formatted Csv line
	 */
	public final String formatFieldList(List<? extends Object> fields, ICsvDefinition lineDef, int[] fieldTypes) {
		
		if (fields == null || fields.size() == 0) {
			return "";
		}
		
		String[] flds = new String[fields.size()];
		
		int min = 0;
		if (fieldTypes != null) {
	    	min = Math.min(fieldTypes.length, flds.length);
			for (int i = 0; i < min; i++) {
	    		flds[i] = formatField(toString(fields.get(i)), fieldTypes[i], lineDef);
	    	}
		}
		for (int i = min; i < flds.length; i++) {
			flds[i] = formatField(toString(fields.get(i)), Type.NT_TEXT, lineDef);
		}
		return formatFieldArray(flds, lineDef);
	}
	
	private String toString(Object o) {
		String s = "";
		if (o != null) {
			s = o.toString();
		}
		return s;
	}

	/**
	 * Format an array of fields as a Line. There is no formatting of fields (it is assumed that this has already 
	 * been done).
	 * 
	 * @param fields fields to format as a Csv Line
	 * @param lineDef Csv Line definition
	 * 
	 * @return Formatted Csv Line
	 */
	protected final String formatFieldArray(String[] fields, ICsvDefinition lineDef) {
		if (fields == null || fields.length == 0) {
			return "";
		}
		StringBuffer buf = new StringBuffer(fields[0]);
	    String delimiter = lineDef.getDelimiter();
		for (int i = 1; i < fields.length; i++) {
	        buf.append(delimiter);
	        if (fields[i] != null) {
	            buf.append(fields[i]);
	        }
	    }
	
	    if (lineDef.getDelimiterOrganisation() != ICsvDefinition.NORMAL_SPLIT && lineDef.getFieldCount() > 0) {
	    	int en = lineDef.getFieldCount();
	    	if (lineDef.getDelimiterOrganisation() == ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END) {
	    		en += 1;
	    	}
	
	    	for (int i = fields.length; i < en; i++) {
	            buf.append(delimiter);
	    	}
	    }
	
	    return buf.toString();
	}
	
	protected abstract String formatField(String s, int fieldType, ICsvDefinition lineDef);
}
