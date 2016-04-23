/*
 * Created on 19/12/2004
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
      
package net.sf.JRecord.Common;

/**
 * Error class for Jrecord / RecordEditor
 *
 * @author Bruce Martin
 * @version 0.51
 */
@SuppressWarnings("serial")
public class RecordException extends RuntimeException {


	/**
	 * @param msg Error Message
	 */
	public RecordException(final boolean x, final String msg) {
		super(msg);
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg));
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg, String parm) {
		this(msg, new Object[] {parm});
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg, Object[] parms) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parms));
	}


	public RecordException(String msg, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg), exception);
	}

	public RecordException(String msg, Object[] parms, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parms), exception);
	}

}
