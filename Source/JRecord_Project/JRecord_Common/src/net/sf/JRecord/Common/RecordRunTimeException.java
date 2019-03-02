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

@SuppressWarnings("serial")
public class RecordRunTimeException extends RuntimeException {


	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg) {
		super(BasicJrTranslation.getTrans().convert(BasicJrTranslation.ST_ERROR, msg));
	}

	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg, String parm) {
		this(msg, new Object[] {parm});
	}

	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg, Object[] parms) {
		super(BasicJrTranslation.getTrans().convertMsg(BasicJrTranslation.ST_ERROR, msg, parms));
	}

	protected RecordRunTimeException(boolean v, String msg) {
		super(msg);
	}
}
