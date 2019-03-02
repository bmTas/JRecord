/*
 * Created on 8/01/2005
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - added null checkin (on screen log variable) to prevent exception
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

import java.awt.Component;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * Displays the log details in a JScollPane.
 *
 * @author Bruce Martin
 *
 */
@SuppressWarnings("serial")
public class ScreenLog extends JScrollPane implements AbsSSLogger {

	private JTextArea log = new JTextArea();

	private int currErrorLevel = 0;

	/**
	 * Implements a Log (via a screen field)
	 *
	 * @param parentContainer parent container
	 */
	public ScreenLog(final Component parentContainer) {
		super();

		//this.parent = parentContainer;
		this.setViewportView(log);
	}


	/**
	 * @see net.sf.JRecord.Log#logException(int, java.lang.Exception)
	 */
	public void logException(int level, Exception ex) {

		if (ex != null && currErrorLevel < level) {
			StringBuffer buf = new StringBuffer(log.getText());
			
			CommonCode.append(buf, ex);

			log.setText(buf.toString());
			//parent.setVisible(true);
		}
	}


	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {

		if (currErrorLevel < level) {
			log.setText(log.getText() + "\n\n" + msg);
		}
	}


	/**
	 * @see net.sf.JRecord.Log#setReportLevel(int)
	 */
	public void setReportLevel(int level) {

		currErrorLevel = level;
	}


}
