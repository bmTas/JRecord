/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zExamples.cobol.toCsv;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.utilityClasses.ParseArguments;




/**
 * Parse argument for Cobol2Csv programs
 *
 * @author Bruce Martin
 *

 */
public class ParseArgsCobol2Csv implements IUpdateFieldName {

    public static final String ARG_COPYBOOK     = "-C";
    public static final String ARG_BINARY       = "-B";
    public static final String ARG_INPUT_FONT   = "-IC";
    public static final String ARG_OUTPUT_FONT  = "-OC";
    public static final String ARG_STRUCTURE    = "-FS";
    public static final String ARG_INPUT_STRUCTURE  = "-IFS";
    public static final String ARG_OUTPUT_STRUCTURE = "-OFS";
    public static final String ARG_IN_FILE      = "-I";
    public static final String ARG_OUT_FILE     = "-O";
    public static final String ARG_SEPARATOR    = "-D";
    public static final String ARG_QUOTE        = "-Q";
    public static final String ARG_RENAME       = "-Rename";
    public static final String ARG_CSV_PARSER   = "-CsvParser";
    public static final String ARG_COPYBOOK1    = "-Copybook";
    public static final String ARG_BINARY1      = "-Dialect";
    public static final String ARG_INPUT_FONT1  = "-InputCharacterSet";
    public static final String ARG_OUTPUT_FONT1 = "-OutputCharacterSet";
    public static final String ARG_STRUCTURE1   = "-FileStructure";
    public static final String ARG_INPUT_STRUCTURE1    = "-InputFileStructure";
    public static final String ARG_OUTPUT_STRUCTURE1   = "-OutputFileStructure";
    public static final String ARG_IN_FILE1     = "-InputFile";
    public static final String ARG_OUT_FILE1    = "-OutputFile";
    public static final String ARG_SEPARATOR1   = "-Delimiter";
    public static final String ARG_QUOTE1       = "-Quote";

    public static final int RO_LEAVE_ASIS = 0;
    public static final int RO_CHANGE_MINUS_TO_UNDERSCORE = 1; 
    public static final int RO_DROP_MINUS = 2;

    private static final String[] VALID_PARAMS = {
            ARG_COPYBOOK, ARG_BINARY, ARG_STRUCTURE, ARG_SEPARATOR,
            ARG_IN_FILE, ARG_OUT_FILE, ARG_INPUT_FONT, ARG_OUTPUT_FONT, ARG_QUOTE, 
            ARG_RENAME, ARG_CSV_PARSER,
            ARG_COPYBOOK1, ARG_BINARY1, ARG_STRUCTURE1, ARG_SEPARATOR1,
            ARG_IN_FILE1, ARG_OUT_FILE1, ARG_INPUT_FONT1, ARG_OUTPUT_FONT1, ARG_QUOTE1,
            ARG_INPUT_STRUCTURE, ARG_INPUT_STRUCTURE1,
            ARG_OUTPUT_STRUCTURE, ARG_OUTPUT_STRUCTURE1,
    };

    public final boolean infilePresent;
    public final int binFormat, inputFileStructure, csvParser, outputFileStructure;
    public final String infile,  outfile,
    					inFont,  outFont,  sep,   quote,
    					copybookName;

    private final static Option[] renameOptions = getRenameOptions();
    private final static Option[] csvParserOption = getCsvParserOptions(); 


    
    public final int renameOption;
    
    
    public ParseArgsCobol2Csv(String[] arguments) {
    	this(false, arguments);
 //   	this(Constants.IO_DEFAULT, Constants.IO_UNICODE_NAME_1ST_LINE, arguments);
   }

    
    /**
     * convert a cobol file to a CSV file
     * @param arguments program arguments
     */
    public ParseArgsCobol2Csv(boolean toCsv, String[] arguments) { 
        ParseArguments args = new ParseArguments(VALID_PARAMS, arguments);
 	
	    String tSep     = args.get2Args(ARG_SEPARATOR1, ARG_SEPARATOR, "\t");
	    String tOutfile = args.get2Args(ARG_OUT_FILE1, ARG_OUT_FILE, "");
	    String tQuote   = args.get2Args(ARG_QUOTE1, ARG_QUOTE, "\"");
	    String renameOptStr = args.getArg(ARG_RENAME, "");
	    binFormat  = ExternalConversion.getDialect(args.get2Args(ARG_BINARY1, ARG_BINARY,  Integer.toString(ICopybookDialects.FMT_MAINFRAME)));
	    infile  = args.get2Args(ARG_IN_FILE1, ARG_IN_FILE, "");
	    outfile = args.get2Args(ARG_OUT_FILE1, ARG_OUT_FILE, "");
	    inFont  = args.get2Args(ARG_INPUT_FONT1, ARG_INPUT_FONT,"");
	    outFont = args.get2Args(ARG_OUTPUT_FONT1, ARG_OUTPUT_FONT, "");
	    copybookName = args.get2Args(ARG_COPYBOOK1, ARG_COPYBOOK, "");
	    String  fStruct = args.get2Args(ARG_STRUCTURE, ARG_STRUCTURE1, "");
	    String  csvDef = Integer.toString(Constants.IO_UNICODE_NAME_1ST_LINE);
	    String  cblDef = fStruct.length()>0 ? fStruct : Integer.toString(Constants.IO_DEFAULT);
	    String  inDefault = cblDef, 
	    		outDefault = csvDef; 
	    if (toCsv) {
	    	inDefault = csvDef;
	    	outDefault = cblDef;
	    }
	    inputFileStructure = ExternalConversion.getFileStructure(0, args.get2Args(ARG_INPUT_STRUCTURE, ARG_INPUT_STRUCTURE1, inDefault));
	    outputFileStructure = ExternalConversion.getFileStructure(
	    		0, 
	    		args.get2Args(
	    				ARG_OUTPUT_STRUCTURE, 
	    				ARG_OUTPUT_STRUCTURE1, 
	    				outDefault));
	    csvParser = getOptionCode(args.getArg(ARG_CSV_PARSER, ""), csvParserOption, CsvParserManagerChar.STANDARD_CSV_PARSER);

	    if ("doublequote".equalsIgnoreCase(tQuote)) {
	    	tQuote = "\"";
	    } else if ("singlequote".equalsIgnoreCase(tQuote)) {
	    	tQuote = "'";
	    }
	    quote = tQuote;
	    infilePresent = present(infile);
	    if (! infilePresent) {
	        usage(" You must supply an input file (-i parameter ");
	    } else {
	        if ("space".equalsIgnoreCase(tSep)) {
	        	tSep = " ";
	        } else if ("tab".equalsIgnoreCase(tSep) || "<tab>".equalsIgnoreCase(tSep) || "\\t".equalsIgnoreCase(tSep)) {
	        	tSep = "\t";
	        } else if ("bar".equalsIgnoreCase(tSep) || "\\|".equalsIgnoreCase(tSep)) {
	        	tSep = "|";
	        }

	        if (! present(tOutfile)) {
	        	tOutfile = infile + ".csv";
	        }
	    }
	    sep = tSep;

	    
	    renameOption = getOptionCode(renameOptStr, renameOptions, RO_CHANGE_MINUS_TO_UNDERSCORE);

    }
    
    private int getOptionCode(String s, Option[] options, int defaultCode) {
    	int ret = defaultCode;
    	if (s.length() > 0) {
	    	boolean searching = true;
	    	for (int i = 0; i < options.length; i++) {
	    		if (options[i].display.equalsIgnoreCase(s)) {
	    			searching = false;
	    			ret = options[i].code;
	    			break;
	    		}
	    	}
	    	
	    	if (searching) {
	    		try {
	    			ret = Integer.parseInt(s);
	    		} catch (Exception e) {
				}
	    	}
	    }
    	
    	return ret;
    }

    
    /* (non-Javadoc)
	 * @see net.sf.JRecord.zExamples.cobol.toCsv.UpdateFieldName#fixName(java.lang.String)
	 */
    @Override
	public final  String updateName(String name) {
        StringBuilder newName = new StringBuilder(name);

        switch (renameOption) {
		case RO_CHANGE_MINUS_TO_UNDERSCORE:
	        if (newName.charAt(newName.length() -1) == ')') {
	        	newName.setLength(newName.length() - 1);
	        }
	        Conversion.replace(newName, "-", "_");
	        Conversion.replace(newName, " ", "_");
	        Conversion.replace(newName, "(", "_");
	        Conversion.replace(newName, ",", "_");
	        Conversion.replace(newName, ")", "_");
	        Conversion.replace(newName, "___", "_");
	        Conversion.replace(newName, "__", "_");
	        break;
		case RO_DROP_MINUS:
			Conversion.replace(newName, "-", "");
			Conversion.replace(newName, " ", "");
        }
      
       return newName.toString();
    }
    /**
     * test if a field has a value
     * @param s value being tested
     * @return wether there is a value there
     */
    private static boolean present(String s) {
        return (s != null &&  ! "".equals(s));
    }


    /**
     * Print parameters
     *
     * @param msg error message
     */
    private static void usage(String msg) {

    	ExternalConversion.getDialectName(ICopybookDialects.FMT_INTEL);
    	
        System.out.println();
        System.out.println(msg);
        System.out.println();
        System.out.println("Program arguments:");
        System.out.println();
        System.out.println("    " + ARG_IN_FILE     + "   or " + ARG_IN_FILE1     + " \t: Input file");
        System.out.println("    " + ARG_OUT_FILE    + "   or " + ARG_OUT_FILE1    + " \t: Output file");
        System.out.println("    " + ARG_COPYBOOK    + "   or " + ARG_COPYBOOK1    + " \t: Cobol Copybook file");
        System.out.println("    " + ARG_SEPARATOR   + "   or " + ARG_SEPARATOR1   + " \t: Field Separator (space, tab or value)");
        System.out.println("    " + ARG_QUOTE       + "  or " + ARG_QUOTE1       + " \t: Quote, you can use DoubleQuote SingleQuote as well");
        System.out.println("    " + ARG_INPUT_FONT  + "  or " + ARG_INPUT_FONT1  + "\t: Input font or character set");
        System.out.println("    " + ARG_OUTPUT_FONT + "  or " + ARG_OUTPUT_FONT1 + "\t: Output font or character set");
        System.out.println("    " + ARG_INPUT_STRUCTURE   + "  or " + ARG_INPUT_STRUCTURE1   + "\t: Input File Structure:");
        System.out.println("    " + ARG_OUTPUT_STRUCTURE   + " or " + ARG_OUTPUT_STRUCTURE1  + "\t: Output File Structure:");

        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_DEFAULT)       + "\t: Determine by  ");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_TEXT_LINE)     + "\t: Use Standard Text IO ");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_FIXED_LENGTH)
                + "\t: Fixed record Length binary ");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_BINARY_IBM_4680)
                + "\t: Binary File, length based on record ");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_VB)
                + "\t: Mainframe VB File "); 
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_VB_DUMP)
                + "\t: Mainframe VB File including BDW (block descriptor word)");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_VB_FUJITSU)
                + "\t: Fujitsu Cobol VB File "); 
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_VB_GNU_COBOL)
                + "\t: Gnu Cobol VB File "); 
        //System.out.println("        " +  Common.NAME_1ST_LINE_IO
        //      + " : CSV file with names on first line");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_UNICODE_NAME_1ST_LINE)
        		+ "\tStandard Csv file with colum names on the first line");
        System.out.println("        " + ExternalConversion.getFileStructureAsString(0, Constants.IO_UNICODE_TEXT)
        		+ "\tCsv file with out colum names on the first line");

        System.out.println("    " + ARG_BINARY + "\t: Cobol Dialect");
        System.out.println("        " + ICopybookDialects.FMT_INTEL     + "  or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_INTEL)     
        							  + "\t: Intel little endian ");
        System.out.println("        " + ICopybookDialects.FMT_MAINFRAME + "  or " +ExternalConversion.getDialectName(ICopybookDialects.FMT_MAINFRAME)
        							  + "\t: Mainframe big endian (Default) ");
        System.out.println("        " + ICopybookDialects.FMT_GNU_COBOL + "  or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_GNU_COBOL) + "\t: Gnu:Cobol ");
        System.out.println("        " + ICopybookDialects.FMT_FUJITSU + " or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_FUJITSU)    + "\t: Fujitsu Cobol ");
        System.out.println("    " + ARG_RENAME + "  : How to update cobol variable names");       
        printOtionArray(renameOptions);

        System.out.println("    " + ARG_CSV_PARSER + "  : Controls how Csv fields are parsed");       
        printOtionArray(csvParserOption);
    }
    
    private static void printOtionArray(Option[] optArray) {
    	for (Option o : optArray) {
    		System.out.println("        " + o.code + " or " +  o.display  + "\t: " + o.description);
    	}
    }
    
    public static Option[] getRenameOptions() {
        Option[] options = {
       		new Option(RO_LEAVE_ASIS, "Asis", "Use the COBOL name"),
    		new Option(RO_LEAVE_ASIS, "Leave_Asis", "Use the COBOL name"),
       		new Option(RO_CHANGE_MINUS_TO_UNDERSCORE, "_", "Change '-(),' to '_'"), 
    		new Option(RO_CHANGE_MINUS_TO_UNDERSCORE, "Change_Minus_To_Underscore", "Change '-(),' to '_'"), 
    		new Option(RO_DROP_MINUS, "No-", "Drop minus ('-') from the name"),
    		new Option(RO_DROP_MINUS, "Drop_Minus", "Drop minus ('-') from the name"),
       };
        
        return options;
    }
    
    
    public static Option[] getCsvParserOptions() {
    	    Option[] options = {
    			new Option(CsvParserManagerChar.BASIC_CSV_PARSER,        "Basic_Parser",    "Parse Csv - when a field starts with \" look for \"<FieldSeparator> or \"<eol> "),
    			new Option(CsvParserManagerChar.STANDARD_CSV_PARSER,     "Standard_Parser", "Parse CSV matching Quotes" ),
    			new Option(CsvParserManagerChar.DB_CSV_PARSER, "Standard_Parse_Quote_4_Char_Fields", "Standard Parser, add Quotes to all Char fields"),
    			new Option(CsvParserManagerChar.BASIC_QUOTED_COL_NAME_CSV_PARSER,    "Basic_Parser_Column_names_in_quotes",    "Basic Parser, Field (Column) names in Quotes"),
    			new Option(CsvParserManagerChar.STANDARD_QUOTED_COL_NAME_CSV_PARSER, "Standard_Parser_Column_names_in_quotes", "Standard Parser, Field (Column) names in Quotes"),
    			new Option(CsvParserManagerChar.DB_QUOTED_COL_NAME_CSV_PARSER,       "Standard_Parser_Quote_4_Char_Fields_Column_names_in_quotes", "Standard Parser, Char fields in Quotes,  Field (Column) names in Quotes"),
    			new Option(CsvParserManagerChar.BASIC_ENSURE_CORRECT_NO_FIELDS,      "Basic_Parser_Delimiter_all_fields",      "Basic Parser, Field Separator for all fields"),
    			new Option(CsvParserManagerChar.BASIC_ENSURE_CORRECT_NO_FIELDS_P1,   "Basic_Parser_Delimiter_all_fields+1",    "Basic Parser, Field Separator for all fields + extra Separator at the End-of-Line"),
    	    };
        
        return options;
    }

    public static final class Option {
    	public final int code;
    	public final String display, description;
		public Option(int code, String display, String description) {
			super();
			this.code = code;
			this.display = display;
			this.description = description;
		}
    	
    	
    }
}
