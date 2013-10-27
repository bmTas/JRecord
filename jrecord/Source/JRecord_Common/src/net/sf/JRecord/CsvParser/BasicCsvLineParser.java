/*
 * @Author Bruce Martin
 * Created on 13/04/2007
 *
 * Purpose:
 */
package net.sf.JRecord.CsvParser;

import java.util.StringTokenizer;

/**
 * Basic CSV line parser. Basically
 *
 *   - If the Field start with {Quote}; the fields is ended by {Quote}{Field-Seperator}
 *   - Otherwise the field ends with a {Field-Seperator}
 *
 * @author Bruce Martin
 *
 */
public class BasicCsvLineParser extends BaseCsvLineParser implements ICsvLineParser {

    private static BasicCsvLineParser instance = new BasicCsvLineParser(false);
	public final int delimiterOrganisation;


    public BasicCsvLineParser(boolean quoteInColumnNames) {
    	this(quoteInColumnNames, ICsvDefinition.NORMAL_SPLIT, false);
    }



	public BasicCsvLineParser(boolean quoteInColumnNames, int delimiterOrganisation) {
		this(quoteInColumnNames, delimiterOrganisation, false);
	}


	public BasicCsvLineParser(boolean quoteInColumnNames, int delimiterOrganisation, boolean allowReturnInFields) {
		super(quoteInColumnNames, allowReturnInFields);
		this.delimiterOrganisation = delimiterOrganisation;
	}


	/**
     * Get the field Count
     *
     * @param line line to inspect
	 * @param lineDef Csv Definition
     * @return the number of fields
     */
    public int getFieldCount(String line, ICsvDefinition lineDef) {
        String[] fields = split(line, lineDef, 0);

        if (fields == null) {
            return 0;
        }

        return fields.length;
    }

    /**
     * Get a specific field from a line
     *
     * @see ICsvLineParser#getField(int, String, String, String)
     */
    public String getField(int fieldNumber, String line, ICsvDefinition lineDef) {
        String[] fields = split(line, lineDef, fieldNumber);

        if (fields == null  || fields.length <= fieldNumber || fields[fieldNumber] == null) {
            return null;
        }
        String quote = lineDef.getQuote();

        if (isQuote(quote)
        && fields[fieldNumber].startsWith(quote)
        && fields[fieldNumber].endsWith(quote)) {
        	String v = "";

        	if (fields[fieldNumber].length() >= quote.length() * 2) {
	            int quoteLength = quote.length();
				v = fields[fieldNumber].substring(
						quoteLength, fields[fieldNumber].length() - quoteLength
				);
	        }
        	fields[fieldNumber] = v;
        }

        return fields[fieldNumber];
    }

    /**
     * @see ICsvLineParser#setField(int, int, String, String, String, String)
     */
    public String setField(int fieldNumber, int fieldType, String line, ICsvDefinition lineDef,
            String newValue) {

        int i;
        String s = newValue;
        StringBuffer buf;
        String[] fields = split(line, lineDef, fieldNumber);
        String quote = lineDef.getQuote();

        if (fields == null || fields.length == 0) {
            //record = new byte[0];
            fields = initArray(fieldNumber + 1);
        }/* else if (fields.length < fieldNumber) {
            String[] hold = fields;
            fields = initArray(fieldNumber + 1);

            for (i = 0; i < hold.length; i++) {
                fields[i] = hold[i];
            }
        }*/

        /*if (fieldNumber > 2) {
        	System.out.println("Quote Details >> >" + quote
        		+ "  >" + s + "<  delim >" + delimiter
        		+ "  > " + s.indexOf(delimiter));
        }*/
        if (quote != null && ! "".equals(quote)
//        && ! (s.startsWith(quote) && (s.endsWith(quote)))
        && (   s.indexOf(lineDef.getDelimiter()) >= 0)
//        	|| s.indexOf(quote) >= 0
        	|| s.startsWith(quote)
        	|| s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0) {
            s = quote + s + quote;
        }

        fields[fieldNumber] = s;

        buf = new StringBuffer(fields[0]);
        for (i = 1; i < fields.length; i++) {
            buf.append(lineDef.getDelimiter());
            if (fields[i] != null) {
                buf.append(fields[i]);
            }
        }

        if (lineDef.getDelimiterOrganisation() != ICsvDefinition.NORMAL_SPLIT && lineDef.getFieldCount() > 0) {
        	int en = lineDef.getFieldCount();
        	if (lineDef.getDelimiterOrganisation() == ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END) {
        		en += 1;
        	}

        	for (i = fields.length; i < en; i++) {
                buf.append(lineDef.getDelimiter());
        	}
        }

        return buf.toString();
    }

    /**
     * Initialise array
     * @param count array size
     * @return initialised array
     */
    private String[] initArray(int count) {
        String[] ret = new String[count];

        for (int i = 0; i < count; i++) {
            ret[i] = "";
        }

        return ret;
    }


	/**
	 * Split a supplied line into its Fields. This only applies to
	 * Comma / Tab seperated files
	 *
	 * @param line line to be split
	 * @param lineDefinition Csv Definition
	 * @param min minimum number of elements in the array
	 *
	 * @return Array of fields
	 */
	public final String[] split(String line, ICsvDefinition lineDefinition, int min) {

		if ((lineDefinition.getDelimiter() == null || line == null)
		||  ("".equals(lineDefinition.getDelimiter()))) {
			return null;
		}

		int i = 0;
		StringTokenizer tok;
		int len, newLength, j;
		String[] temp, ret;
		boolean keep = true;
		String quote = lineDefinition.getQuote();

		tok = new StringTokenizer(line, lineDefinition.getDelimiter(), true);
		len = tok.countTokens();
		temp = new String[Math.max(len, min)];

		//if (min == 4) System.out.println("}}"+ quote + "< >" + isQuote(quote)+ "{{ ");
		if (! isQuote(quote)) {
		    while (tok.hasMoreElements()) {
		        temp[i] = tok.nextToken();
//		        if (min == 4) System.out.print("->>" + (i) + " " + keep + " >" + temp[i]
//		                        + "< >" + delimiter + "< ");
		        if (lineDefinition.getDelimiter().equals(temp[i])) {
		            if (keep) {
		                temp[i++] = "";
		               // if (min == 4) System.out.print(" clear ");
		            }
		            keep = true;
		        } else {
		            keep = false;
		            i += 1;
		        }
		        //if (min == 4) System.out.println(" >> "  + keep);
		    }
		    if (i < temp.length) {
		    	temp[i] = "";
		    }
		} else {
		    StringBuffer buf = null;
		    String s;
		    boolean building = false;
		    while (tok.hasMoreElements()) {
		        s = tok.nextToken();
		        if (building) {
		            buf.append(s);
		            if (s.endsWith(quote)) {
		                //buf.delete(buf.length() - 1, buf.length());
		                temp[i++] = buf.toString();
		                building = false;
		                //buf.delete(0, buf.length());
		                keep = false;
		            }
		        } else if (lineDefinition.getDelimiter().equals(s)) {
		            if (keep) {
		                temp[i++] = "";
		            }
		            keep = true;
		        } else if (s.startsWith(quote)
		        	   && (! s.endsWith(quote) || s.length() == quote.length())) {
		            buf = new StringBuffer(s);
		            building = true;
		        } else {
		        	//if (min == 4) System.out.println("Split 2 >> " + i + " >>" + s + "<< ");
		           //System.out.println("]] >" + s + "<" + (s.startsWith(quote))
		           //         + " " + (! s.endsWith(quote)) + " " + (s.length() == 1));
		            temp[i++] = s;
		            keep = false;
		        }
		    }
		    if (building) {
//		        if (quote.equals(buf.substring(buf.length() - 1))) {
//		            buf.delete(buf.length() - 1, buf.length());
//		        }
		        temp[i++] = buf.toString();
		    }
		}

		ret = temp;
		newLength = Math.max(i, min + 1);
		if (newLength != temp.length) {
		    ret = new String[newLength];
		    for (j = 0; j < i; j++) {
		        ret[j] = temp[j];
		    }
		    for (j = i; j < newLength; j++) {
		        ret[j] = "";
		    }
		}

		return ret;
	}

	/**
	 * is quote present
	 * @param quote quote string
	 * @return if it defines a quote
	 */
	private boolean isQuote(String quote) {
	    return quote != null && ! "".equals(quote);
	}

    /**
     * Return a basic CSV line parser
     * @return Returns the instance.
     */
    public static BasicCsvLineParser getInstance() {
        return instance;
    }
}
