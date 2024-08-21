/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.recordEditorXml.readWrite;

import net.sf.JRecord.Common.XmlConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 * Purpose: Illistrate how read an XML file using JRecord routines. 
 * 
 * These routines where written to Allow the RecordEditor to Read / Write XML files. You should 
 * consider using standard Java XML libraries instead. But Ifyou want use the RecordEditor Routines
 * This is how to do it.
 * 
 * It actually reads a mainframe CICS SDF map converted to XML
 * 
 * 
 * @author Bruce Martin
 */
public class XmplXmlIO01 {

    private String installDir = TstConstants.SAMPLE_DIRECTORY;
    private String mam0045    = installDir + "Xml/MAM0045_Map.XML";
    
    
    
    public XmplXmlIO01() {
    
    	AbstractLine line;
    	XmlLineReader reader = new XmlLineReader(true);
    	String field, id;

    	System.out.println(
  			  "row\t"
  			+ "col\t"
  			+ "Attributes           ".substring(0, 12) + "\t"
  			+ "Field Name           ".substring(0, 12) + "\t"
  			+ "Initial Value"
    	);
    	
    	System.out.println(
    			"======================================================================================================"
    	);

    	try {
    		reader.open(mam0045);
    		
    		while ((line = reader.read()) != null) {
    			
    			// Checking the XML element  name;  
    			field = line.getFieldValue(XmlConstants.XML_NAME).asString();
                if (field != null && "field".equalsIgnoreCase(field)) {
                	id = line.getFieldValue("id").asString();
                	if (id == null) {
                		id = "";
                	}
                	System.out.println(
                			  line.getFieldValue("row").asString() + "\t"
                			+ line.getFieldValue("col").asString() + "\t"
                			+ (line.getFieldValue("ATTRB").asString() + "                  ").substring(0, 12) + "\t"
                			+ (id + "                  ").substring(0, 12) + "\t"
                			+ line.getFieldValue("INITIAL").asString()
                	);
                }
            }

            reader.close();

    	} catch (Exception e) {
			e.printStackTrace();
		}
    }
    
    
    public static void main(String[] args) {
    	new XmplXmlIO01();
    }

}
