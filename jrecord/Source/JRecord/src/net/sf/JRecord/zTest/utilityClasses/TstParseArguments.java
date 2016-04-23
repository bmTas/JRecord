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

package net.sf.JRecord.zTest.utilityClasses;

import java.util.List;

import net.sf.JRecord.utilityClasses.ParseArguments;
import junit.framework.TestCase;

public class TstParseArguments extends TestCase {

	private static final String[] ARGS = {"-a", "-b", "-c", "-dddd", "-e"};
	private static final String[] MULTI_ARGS = {"-ma", "-mb", "-mc", "-mdddd"};
	
	private static final String[] ARG_VALUES_1 = {"-a", "aa", "-c", "ccc"};
	private static final String[] ARG_VALUES_2 = {"-a", "aa", "-c", "ccc", "-a", "bb", "-a", "cc"};
	private static final String[] ARG_VALUES_3 = {"-a", "aa", "-ma", "a1", "-mb", "b", "-mb", "bb",  "-c", "ccc", "-mb", "bbb"};
	
	
	public void testGetArgString() {
		chkGetArgStr( new ParseArguments(ARGS, ARG_VALUES_1));
		chkGetArgStr( new ParseArguments(ARGS, ARG_VALUES_2));

		chkGetArgStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_1));
		chkGetArgStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_2));
		chkGetArgStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_3));
	}

	/**
	 * @param pa
	 */
	private void chkGetArgStr(ParseArguments pa) {
		assertEquals("aa", pa.getArg("-a"));
		assertEquals("aa", pa.getArg("-A"));
		assertEquals("ccc", pa.getArg("-c"));
		String s = pa.getArg("-b");
		assertTrue("Expecting null, not >" + s, s == null);
	}

	public void testGet2Args() {

		chkGet2Args(new ParseArguments(ARGS, ARG_VALUES_1));
		chkGet2Args(new ParseArguments(ARGS, ARG_VALUES_2));

		chkGet2Args(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_1));
		chkGet2Args(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_2));
		chkGet2Args(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_3));
	}

	/**
	 * @param pa
	 */
	private void chkGet2Args(ParseArguments pa) {
		assertEquals("aa", pa.get2Args("-a", "-c", "zzzz"));
		assertEquals("aa", pa.get2Args("-a", "-b", "zzzz"));
		assertEquals("aa", pa.get2Args("-b", "-a", "zzzz"));
		assertEquals("zzzz", pa.get2Args("-b", "-e", "zzzz"));

		assertEquals("ccc", pa.get2Args("-c", "-a", "zzz"));
		assertEquals("ccc", pa.get2Args("-c", "-b", "zzz"));
		assertEquals("ccc", pa.get2Args("-b", "-c", "zzz"));
	}

	public void testGetArgStringString() {
		chkGetArgStrStr( new ParseArguments(ARGS, ARG_VALUES_1));
		chkGetArgStrStr( new ParseArguments(ARGS, ARG_VALUES_2));

		chkGetArgStrStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_1));
		chkGetArgStrStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_2));
		chkGetArgStrStr(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_3));
	}
	

	/**
	 * @param pa
	 */
	private void chkGetArgStrStr(ParseArguments pa) {
		assertEquals("aa", pa.getArg("-a", "-xyz"));
		assertEquals("aa", pa.getArg("-A", "-xyz"));
		assertEquals("ccc", pa.getArg("-c", "-xyz"));
		String s = pa.getArg("-b", "-xyz");
		assertEquals("-xyz", s);
	}


	public void testGetArgList() {
		chkGetArgList( new ParseArguments(ARGS, ARG_VALUES_1));
		chkGetArgList( new ParseArguments(ARGS, ARG_VALUES_2));

		chkGetArgList(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_1));
		chkGetArgList(new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_2));
		
		ParseArguments pa = new ParseArguments(ARGS, MULTI_ARGS, ARG_VALUES_3);
		chkGetArgList(pa);
		chkListItem("a1", pa.getArgList("-ma"));
		List<String> argValues = pa.getArgList("-mb");
		assertEquals(3, argValues.size());
		assertEquals("b", argValues.get(0));
		assertEquals("bb", argValues.get(1));
		assertEquals("bbb", argValues.get(2));
	}
	/**
	 * @param pa
	 */
	private void chkGetArgList(ParseArguments pa) {
		chkListItem("aa", pa.getArgList("-a"));
		chkListItem("aa", pa.getArgList("-A"));
		chkListItem("ccc", pa.getArgList("-c"));
		List<String> l = pa.getArgList("-b");
		assertTrue("Expecting null, not >" + l, l == null || l.size() == 0);
	}
	
	private void chkListItem(String expected, List<String> l) {
		assertEquals(1, l.size());
		assertEquals(expected, l.get(0));
	}

}
