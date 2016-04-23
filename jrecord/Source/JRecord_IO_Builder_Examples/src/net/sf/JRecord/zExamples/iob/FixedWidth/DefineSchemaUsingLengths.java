/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord IOBuilder examples
 *    
 *    Sub-Project purpose: Examples of using JRecord IOBuilders
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
      
package net.sf.JRecord.zExamples.iob.FixedWidth;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;


/**
 * This class defines a Schema using Field Lengths and letting JRecord calculate positions
 * 
 * @author Bruce Martin
 *
 */
public class DefineSchemaUsingLengths {

	public DefineSchemaUsingLengths() {
		try {
			AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
									.defineFieldsByLength()
										.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
										.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
										.addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
										.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
										.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
										.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
									.endOfRecord()
									.newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
			AbstractLine saleRecord;
			
            while ((saleRecord = reader.read()) != null) {
                System.out.println(
                				 saleRecord.getFieldValue("Sku").asString()
                        + "\t" + saleRecord.getFieldValue("Store").asString()
                        + "\t" + saleRecord.getFieldValue("Date").asString()
                        + "\t" + saleRecord.getFieldValue("Dept").asString()
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
		new DefineSchemaUsingLengths();
	}
}
