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
      
package net.sf.JRecord.zExamples.noCopybook;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;


/**
 * This class defines a Schema using Field positions and letting JRecord calculate lengths
 * 
 * @author Bruce Martin
 *
 */
public class DefineSchemaUsingPosition {

	public DefineSchemaUsingPosition() {
		try {
			ExternalRecord r =
					ExternalRecord
						.newFixedWidthRecord("My_Record", IFileStructureConstants.IO_TEXT_LINE, "")
							.addFieldByPosition("Sku"  , Type.ftChar             ,  1, 0)
							.addFieldByPosition("Store", Type.ftNumRightJustified,  9, 0)
							.addFieldByPosition("Date" , Type.ftNumRightJustified, 12, 0)
							.addFieldByPosition("Dept" , Type.ftNumRightJustified, 18, 0)
							.addFieldByPosition("Qty"  , Type.ftNumRightJustified, 21, 0)
							.addFieldByPosition("Price", Type.ftNumRightJustified, 23, 6, 2)
						.asExternalRecord(); 
							// Need to define both position and length for the last field
			LayoutDetail schema = r.asLayoutDetail();
			AbstractLine saleRecord;
			AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(schema);
			
			reader.open(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile(), schema);
			
            while ((saleRecord = reader.read()) != null) {
                System.out.println(saleRecord.getFieldValue("Sku").asString()
                        + "\t" + saleRecord.getFieldValue("Qty").asString()
                        + "\t" + saleRecord.getFieldValue("Price").asString());

            }
            reader.close();
		} catch (RecordException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		new DefineSchemaUsingPosition();
	}
}
