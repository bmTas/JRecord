/*
 * @Author Bruce Martin
 * Created on 13/04/2007
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.CsvParser;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import net.sf.JRecord.Types.Type;

/**
 * Basic CSV line parser. Basically
 *
 *   - If the Field start with {Quote}; the fields is ended by {Quote}{Field-Seperator}
 *   - Otherwise the field ends with a {Field-Separator}
 *
 * @author Bruce Martin
 *
 */
public class BasicCsvLineParser extends BaseCsvLineParser {

    private static BasicCsvLineParser instance = new BasicCsvLineParser(false);
	public final int delimiterOrganisation;
	protected final boolean textFieldsInQuotes;


    public BasicCsvLineParser(boolean quoteInColumnNames) {
    	this(quoteInColumnNames, ICsvDefinition.NORMAL_SPLIT, false, false);
    }



	public BasicCsvLineParser(boolean quoteInColumnNames, int delimiterOrganisation) {
		this(quoteInColumnNames, delimiterOrganisation, false, false);
	}

	public BasicCsvLineParser(boolean quoteInColumnNames, int delimiterOrganisation, boolean allowReturnInFields, boolean textFieldsInQuotes) {
		super(quoteInColumnNames, allowReturnInFields);
		this.delimiterOrganisation = delimiterOrganisation;
		this.textFieldsInQuotes = textFieldsInQuotes;
	}


	/**
     * Get the field Count
     *
     * @param line line to inspect
	 * @param lineDef Csv Definition
     * @return the number of fields
     */
    public final int getFieldCount(String line, ICsvDefinition lineDef) {
        String[] fields = splitInternal(line, lineDef, 0);

        if (fields == null) {
            return 0;
        }

        return fields.length;
    }

    /**
     * Get a specific field from a line
     *
     * @see ICsvCharLineParser#getField(int, String, ICsvDefinition)
     */
    public final String getField(int fieldNumber, String line, ICsvDefinition lineDef) {
        String[] fields = splitInternal(line, lineDef, fieldNumber);

        if (fields == null  || fields.length <= fieldNumber || fields[fieldNumber] == null) {
            return null;
        }
 //       String quote = lineDef.getQuote();

        update4quote(fields, fieldNumber, lineDef.getQuoteDefinition().asString());
//        if (isQuote(quote)
//        && fields[fieldNumber].startsWith(quote)
//        && fields[fieldNumber].endsWith(quote)) {
//        	String v = "";
//
//        	if (fields[fieldNumber].length() >= quote.length() * 2) {
//	            int quoteLength = quote.length();
//				v = fields[fieldNumber].substring(
//						quoteLength, fields[fieldNumber].length() - quoteLength
//				);
//	        }
//        	fields[fieldNumber] = v;
//        }

        return fields[fieldNumber];
    }
    
    

    @Override
	public final List<String> getFieldList(String line, ICsvDefinition csvDefinition) {
    	String[] fields = splitInternal(line, csvDefinition, 0);
    	if (fields == null) {
            return new ArrayList<String>(1);
        }
		String quote = csvDefinition.getQuoteDefinition().asString();
		ArrayList<String> ret =  new ArrayList<String>(fields.length);
		for (int i = 0; i < fields.length; i++) {
			update4quote(fields, i, quote);
			ret.add(fields[i]);
		}
		
		return ret;
	}


    protected void update4quote(String[] fields, int fieldNumber, String quote ) {
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

    }

	/**
     * @see ICsvCharLineParser#setField(int, int, String, ICsvDefinition, String)
     */
    public final String setField(int fieldNumber, int fieldType, String line, ICsvDefinition lineDef,
            String newValue) {

        String[] fields = splitInternal(line, lineDef, fieldNumber);

        if (fields == null || fields.length == 0) {
            fields = initArray(fieldNumber + 1);
        }

        fields[fieldNumber] = formatField(newValue, fieldType, lineDef);
        
        return formatFieldArray(fields, lineDef);
    }
    
    protected String formatField(String s, int fieldType, ICsvDefinition lineDef) {
    	String quote = lineDef.getQuoteDefinition().asString();
    	if (s == null) {
    		s = "";
    	} else if (quote != null && quote.length() > 0
		        && (   (textFieldsInQuotes && (fieldType != Type.NT_NUMBER))
		        	||	s.indexOf(super.getDelimFromCsvDef(lineDef)) >= 0
		//        	|| s.indexOf(quote) >= 0
		        	|| s.startsWith(quote)
		        	|| s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0)) {
            s = quote + s + quote;
        }
        return s;
    }
    /**
     * Initialize array
     * @param count array size
     * @return initialized array
     */
    private String[] initArray(int count) {
        String[] ret = new String[count];

        for (int i = 0; i < count; i++) {
            ret[i] = "";
        }

        return ret;
    }


    public final String[] split(String line, ICsvDefinition lineDefinition, int min) {
    	String[] fields = splitInternal(line, lineDefinition, min);
		String quote = lineDefinition.getQuoteDefinition().asString();
		for (int i = 0; i < fields.length; i++) {
			update4quote(fields, i, quote);
		}
		return fields;
    }
    
	/**
	 * Split a supplied line into its Fields. This only applies to
	 * Comma / Tab separated files
	 *
	 * @param line line to be split
	 * @param lineDefinition Csv Definition
	 * @param min minimum number of elements in the array
	 *
	 * @return Array of fields
	 */
	private final String[] splitInternal(String line, ICsvDefinition lineDefinition, int min) {

		String delimiter = super.getDelimFromCsvDef(lineDefinition);
		if ((delimiter == null || line == null)
		||  ("".equals(delimiter))) {
			return null;
		}

		int i = 0;
		StringTokenizer tok;
		int len, newLength, j;
		String[] temp, ret;
		boolean keep = true;
		String quote = lineDefinition.getQuoteDefinition().asString();

		tok = new StringTokenizer(line, delimiter, true);
		len = tok.countTokens();
		temp = new String[Math.max(len, min)];

		//if (min == 4) System.out.println("}}"+ quote + "< >" + isQuote(quote)+ "{{ ");
		if (! isQuote(quote)) {
		    while (tok.hasMoreElements()) {
		        temp[i] = tok.nextToken();
//		        if (min == 4) System.out.print("->>" + (i) + " " + keep + " >" + temp[i]
//		                        + "< >" + delimiter + "< ");
		        if (delimiter.equals(temp[i])) {
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
		            if (endOfField(buf.length(), s, quote)) {
		                //buf.delete(buf.length() - 1, buf.length());
		          
		                temp[i++] = buf.toString();
		                building = false;
		                //buf.delete(0, buf.length());
		                keep = false;
		            }
		        } else if (delimiter.equals(s)) {
		            if (keep) {
		                temp[i++] = "";
		            }
		            keep = true;
		        } else if (s.startsWith(quote)
		        	   && (! endOfField(0, s, quote))) {
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
		    System.arraycopy(temp, 0, ret, 0, i);
//		    for (j = 0; j < i; j++) {
//		        ret[j] = temp[j];
//		    }
		    for (j = i; j < newLength; j++) {
		        ret[j] = "";
		    }
		}

		return ret;
	}

	
	/**
	 * check if it is end of the fields
	 */
	protected boolean  endOfField(int startsAt, String s, String quote) {
		return startsAt + s.length() > quote.length() && (s.endsWith(quote));
	}

	/**
	 * is quote present
	 * @param quote quote string
	 * @return if it defines a quote
	 */
	protected final boolean isQuote(String quote) {
	    return quote != null && quote.length() > 0;
	}

    /**
     * Return a basic CSV line parser
     * @return Returns the instance.
     */
    public static BasicCsvLineParser getInstance() {
        return instance;
    }
}
