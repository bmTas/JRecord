/*
 * Created on 8/01/2005
 *
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
      
package net.sf.JRecord.Log;

import java.io.PrintStream;

/**
 * A very simple logger that writes details to System.out
 *
 * @author Bruce Martin
 *
 */
public class TextLog implements AbsSSLogger {
	
	private final PrintStream outStream;

	public TextLog() {
		this(System.err);
	}
	
	public TextLog(PrintStream outStream) {
		this.outStream = outStream;
	}

	/**
	 * @see net.sf.JRecord.Log#setReportLevel(int)
	 */
	public void setReportLevel(int level) {
	}

	/**
	 * @see net.sf.JRecord.Log#logException(int, java.lang.Exception)
	 */
	public void logException(int level, Exception ex) {

		outStream.println();
		
		if (ex != null) {
			outStream.println();
			ex.printStackTrace(outStream);
		}
	}
	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {

		outStream.println();
		outStream.println(msg);
	}
	
	public static AbsSSLogger getLog(AbsSSLogger log) {
		if (log == null) {
			log = new TextLog();
		}
		return log;
	}
}