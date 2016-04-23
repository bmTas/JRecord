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

package net.sf.JRecord.utilityClasses;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

/**
 * Generic cllass to parses program arguments into a hashmap
 *
 * @author Bruce Martin
 *
 */
public class ParseArguments {
	
	private static final String[] EMPTY_ARRAY = {}; 

//    private HashMap<String, String> argsMap = new HashMap<String, String>();
    private HashMap<String, List<String>> argsMapOfList = new HashMap<String, List<String>>();


    /**
     * Parse program arguments
     *
     * @param validArgs list of all valid arguments
     *
     * @param args arguments supplied to the program
     */
    public ParseArguments(final String[] validArgs, final String[] args) {
    	this(validArgs, EMPTY_ARRAY, args);
    	
    }
    public ParseArguments(final String[] validSingleItemArgs, final String[] validMultiItemArgs,final String[] args) {

        int i;
        HashSet<String> valid = new HashSet<String>();
        HashSet<String> validMulti = new HashSet<String>();
        String currArg = null;
        StringBuilder currValue = new StringBuilder();
        String sep = "";

        for (i = 0; i < validSingleItemArgs.length; i++) {
            valid.add(validSingleItemArgs[i].toUpperCase());
        }
        for (i = 0; i < validMultiItemArgs.length; i++) {
        	validMulti.add(validMultiItemArgs[i].toUpperCase());
        }

        for (i = 0; i < args.length; i++) {
            if (args[i].startsWith("-")) {
            	updateMap(currArg, currValue.toString());
                
                currValue.setLength(0);;
                sep = "";
                currArg = args[i].toUpperCase();

                if (valid.contains(currArg)) {
                	if (argsMapOfList.containsKey(currArg)) {
                		System.out.println(" ** Only one " + args[i] + " argument is allowed !!!");
                		currArg = null;
                	}
                } else if (! validMulti.contains(currArg) ) {
                    currArg = null;
                    System.out.println(" ** Invalid Argument " + args[i]);
                }
            } else {
                currValue.append(sep).append(args[i]);
                sep = " ";
            }
        }
        updateMap(currArg, currValue.toString());
        
        System.out.println();
        System.out.println();
    }


    private void updateMap(String currArg, String currValue) {
        if (currArg != null) {
        	List<String> list = argsMapOfList.get(currArg);
        	if (list == null) {
        		list = new ArrayList<String>(5);
        	}
        	list.add(currValue);
            argsMapOfList.put(currArg, list);
        }
    }
    /**
     * Get a requested argument
     *
     * @param arg argument being requested
     *
     * @return Argument value
     */
    public String getArg(String arg) {
        return getArg(arg, null);
    }


    /**
     * Get 2 arguments
     * @param arg1 first argument to check
     * @param arg2 2nd argument
     * @param defaultValue The Default value
     * @return requested argument value
     */
    public String get2Args(String arg1, String arg2, String defaultValue) {
    	return getArg(arg1, getArg(arg2, defaultValue));
    }
    	 
    /**
     * Get a requested argument
     *
     * @param arg argument being requested
     * @param defaultValue default argument value
     *
     * @return requested argument value
     */
    public String getArg(String arg, String defaultValue) {
        String ret = defaultValue;
        String key = arg.toUpperCase();
        
        if (argsMapOfList.containsKey(key)) {
            List<String> list = argsMapOfList.get(key);
            if (list.size() > 1) {
            	throw new RuntimeException("There where: " + list.size() + " objects in the list");
            }
			ret = list.get(0);
        }
        return ret;
    }
    
    public List<String> getArgList(String arg) {
    	return argsMapOfList.get(arg.toUpperCase());
    }

//    /**
//     * Get a requested integer argument
//     *
//     * @param arg argument being requested
//     *
//     * @return Argment value
//     * @throws Exception any error that occurs
//     */
//    public int getIntArg(String arg) throws Exception {
//        try {
//            return Integer.parseInt(getArg(arg));
//        } catch (Exception e) {
//            System.out.println();
//            System.out.println("Error processing integer argument " + arg
//                    + "  - " + e.getMessage());
//            System.out.println();
//            throw e;
//        }
//     }
//
//
//    /**
//     * Get a requested integer argument
//     *
//     * @param arg argument being requested
//     * @param defaultVal default value for the parameter
//     * @return Argment value
//      */
//    public int getIntArg(String arg, int defaultVal) {
//        try {
//            return Integer.parseInt(getArg(arg));
//        } catch (Exception e) {
//            return defaultVal;
//        }
//     }
//
//
//    /**
//     * Get a requested integer argument
//     *
//     * @param arg1 argument being requested
//     * @param arg2 argument being requested
//     * @param defaultVal default value for the parameter
//     * @return Argment value
//     */
//    public int get2IntArgs(String arg1, String arg2, int defaultVal) {
//        String strVal = getArg(arg1);
//        if (strVal != null && strVal.length() > 0) {
//            try {
//            	return Integer.parseInt(strVal);
//            } catch (Exception e) { }
//        }
//        
//        strVal = getArg(arg2);
//        if (strVal != null && strVal.length() > 0) {
//            try {
//            	return Integer.parseInt(strVal);
//            } catch (Exception e) { }
//        }
//    
//        return defaultVal;
//     }

}
