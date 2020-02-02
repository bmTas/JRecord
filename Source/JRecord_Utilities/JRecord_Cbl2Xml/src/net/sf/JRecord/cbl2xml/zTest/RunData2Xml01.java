/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml.zTest;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.Data2Xml;

public class RunData2Xml01 {

	public static void main(String[] args) throws RecordException, IOException, XMLStreamException {
		String[] a = {
				"-cobol", "DTAR020.cbl", "-font", "cp037", 
				"-fileOrganisation", "FixedWidth", "-input", "DTAR020.bin",
				"-output", "out/DTAR020_A.xml"
		};
 
		Data2Xml.main(a);
	}

}
