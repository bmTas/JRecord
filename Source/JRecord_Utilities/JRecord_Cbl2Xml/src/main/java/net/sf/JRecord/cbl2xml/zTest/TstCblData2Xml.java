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
import java.net.URL;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.schema.ArrayElementChecks;


public class TstCblData2Xml {

    public static String getFullName(String filename) {
    	URL resource = TstCblData2Xml.class.getResource(filename);
    	if (resource == null) {
    		System.out.println(" --> Can not find: " + filename);
    	}
		return resource.getFile();
    }


	public static void main(String[] args) throws RecordException, IOException, XMLStreamException {
		String fullName = getFullName("StoreSales5.txt");
		ICobol2Xml cbl2xml = Cobol2GroupXml.newCobol2Xml(getFullName("cbl2xml_Test112.cbl"));
		cbl2xml.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		
		cbl2xml.cobol2xml(fullName, "G:/Temp/Store_Sales_5a.xml");
		
		cbl2xml	.setArrayCheck("A-Sale", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Department-Dtls", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Product-details", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Orders", ArrayElementChecks.INSTANCE.newStopAtSpaces())
			.cobol2xml(fullName, "G:/Temp/Store_Sales_5b.xml");
		
		//cbl2xml.xml2Cobol(getFullName("StoreSales5.xml"), "G:/Temp/Store_Sales_5.txt");

	}
}
