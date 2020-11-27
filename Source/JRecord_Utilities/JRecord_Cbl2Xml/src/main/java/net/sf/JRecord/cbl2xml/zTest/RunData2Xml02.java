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
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;

public class RunData2Xml02 {

	public static void main(String[] args) throws RecordException, IOException, XMLStreamException {
		String cbl1 =  Cb2XmlCode.getFullName("cobol/amsPoDownload.cbl");
		String forgText = "text";
		String poRec = "PO-Record";
		String recType = "Record-Type";
		String productRec = "Product-Record";
		String locationRec = "Location-Record";
		String poValue = "H1";
		String productValue = "D1";
		String storeValue = "S1";
		String in1 = Cb2XmlCode.getFullName("Ams_PODownload_20041231.txt");
		String out1 = "G:/Temp/Ams_PODownload_20041231_bat_Tree.xml";

			
		String[] args1 = {
					"-cobol", cbl1, "-fileOrganisation", forgText,
					 "-recordSelection", poRec,       recType + "=" + poValue,
					 "-recordSelection", productRec,  recType + "=" + productValue, "-recordParent", productRec, poRec,
					 "-recordSelection", locationRec, recType + "=" + storeValue,   "-recordParent", locationRec, productRec,
					 "-input", in1,
					 "-output", out1,
		};

		Data2Xml.main(args1);
	}

}
