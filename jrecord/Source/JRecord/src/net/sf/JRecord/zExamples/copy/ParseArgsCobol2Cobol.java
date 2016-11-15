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

package net.sf.JRecord.zExamples.copy;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.utilityClasses.ParseArguments;




/**
 * Parse argument for Cobol2Csv programs
 *
 * @author Bruce Martin
 *

 */
public class ParseArgsCobol2Cobol {

    public static final String ARG_IN_FILE       = "-I";
    public static final String ARG_OUT_FILE      = "-O";
    public static final String ARG_IN_COPYBOOK   = "-InputCopybook";
    public static final String ARG_IN_BINARY     = "-InputDialect";
    public static final String ARG_IN_STRUCTURE  = "-InputFileStructure";
    public static final String ARG_INPUT_FONT    = "-InputCharacterSet";
    public static final String ARG_OUT_COPYBOOK  = "-OutputCopybook";
    public static final String ARG_OUT_BINARY    = "-OutputDialect";
    public static final String ARG_OUT_STRUCTURE = "-OutputFileStructure";
    public static final String ARG_OUTPUT_FONT   = "-OutputCharacterSet";
    public static final String ARG_IN_FILE1      = "-InputFile";
    public static final String ARG_OUT_FILE1     = "-OutputFile";
 

    private static final String[] VALID_PARAMS = {
           
            ARG_IN_FILE, ARG_IN_FILE1, ARG_IN_COPYBOOK, ARG_IN_BINARY, ARG_IN_STRUCTURE, ARG_INPUT_FONT,
            ARG_OUT_FILE, ARG_OUT_FILE1, ARG_OUT_COPYBOOK, ARG_OUT_BINARY, ARG_OUT_STRUCTURE, ARG_OUTPUT_FONT,

    };

    public final boolean infilePresent;
    public final int inBinFormat, inFileStructure,  outBinFormat, outFileStructure;
    public final String infile,  outfile,
    					inFont,  outFont,  
    					inCopybookName, outCopybookName;
    
    
    
    /**
     * convert a cobol file to a CSV file
     * @param arguments program arguments
     */
    public ParseArgsCobol2Cobol(String[] arguments) { 
        ParseArguments args = new ParseArguments(VALID_PARAMS, arguments);

	    String mainframeStr = Integer.toString(ICopybookDialects.FMT_MAINFRAME);
		inBinFormat  = ExternalConversion.getDialect(args.getArg(ARG_IN_BINARY,  mainframeStr));
	    outBinFormat  = ExternalConversion.getDialect(args.getArg(ARG_OUT_BINARY,  mainframeStr));
	    infile  = args.get2Args(ARG_IN_FILE1, ARG_IN_FILE, "");
	    outfile = args.get2Args(ARG_OUT_FILE1, ARG_OUT_FILE, "");
	    inFont  = args.getArg(ARG_INPUT_FONT, "");
	    outFont = args.getArg(ARG_OUTPUT_FONT, ""); 
	    inCopybookName = args.getArg(ARG_IN_COPYBOOK);
	    outCopybookName = args.getArg(ARG_OUT_COPYBOOK);
	    inFileStructure = ExternalConversion.getFileStructure(0, args.getArg(ARG_IN_STRUCTURE, "" + Constants.IO_DEFAULT));
	    outFileStructure = ExternalConversion.getFileStructure(0, args.getArg(ARG_OUT_STRUCTURE, "" + Constants.IO_DEFAULT));


	    infilePresent = present(infile);
	    if (! infilePresent) {
	        usage(" You must supply an input file (-i parameter) ");
	    } 	    

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
        System.out.println("    " + ARG_IN_FILE     + "  or " + ARG_IN_FILE1     + " \t: Input file");
        System.out.println("    " + ARG_IN_COPYBOOK  + "\t: Cobol/Xml Copybook file");
        System.out.println("    " + ARG_IN_STRUCTURE + "\t: Input File Structure (or File-Organisation):");
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
        System.out.println("    " + ARG_INPUT_FONT   + "\t: Input font or character set");
        System.out.println("    " + ARG_IN_BINARY + "\t: Cobol Dialect");
        System.out.println("        " + ICopybookDialects.FMT_INTEL     + " or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_INTEL)     
        							  + "\t: Intel little endian ");
        System.out.println("        " + ICopybookDialects.FMT_MAINFRAME + " or " +ExternalConversion.getDialectName(ICopybookDialects.FMT_MAINFRAME)
        							  + "\t: Mainframe big endian (Default) ");
        System.out.println("        " + ICopybookDialects.FMT_GNU_COBOL + " or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_GNU_COBOL) + "\t: Gnu:Cobol ");
        System.out.println("        " + ICopybookDialects.FMT_FUJITSU + " or " + ExternalConversion.getDialectName(ICopybookDialects.FMT_FUJITSU)    + "\t: Fujitsu Cobol ");
        System.out.println();
        System.out.println("    " + ARG_OUT_FILE      + "  or " + ARG_OUT_FILE1    + " \t: Output file");
        System.out.println("    " + ARG_OUTPUT_FONT   + "\t: Output font or character set");
        System.out.println("    " + ARG_OUT_COPYBOOK  + "\t: Output Cobol/Xml Copybook file");
        System.out.println("    " + ARG_OUT_STRUCTURE + "\t: Output File Structure (or File-Organisation)");
        System.out.println("            See: " + ARG_IN_STRUCTURE + " for details");
        System.out.println("    " + ARG_OUTPUT_FONT   + "\t: Output font or character set");
        System.out.println("    " + ARG_OUT_BINARY    + "\t: Output Cobol Dialect) ");
        System.out.println("            See: " + ARG_IN_BINARY + " for details");
                                                      
        //System.out.println("        " +  Common.NAME_1ST_LINE_IO
        //      + " : CSV file with names on first line");


    }
}
