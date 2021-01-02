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
      
package net.sf.JRecord.zExamples.iob.cobol.iobuilder;

import java.io.FileNotFoundException;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class Dtar020Read {

	public static void main(String[] a) throws FileNotFoundException, IOException {
	
	   ICobolIOBuilder iob = JRecordInterface1.COBOL
            .newIOBuilder("G:/Users/Bruce01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl")
                 .setFont("cp037");
    
	   AbstractLine line;
	   Dtar020.ClassDtar020 dtar020 = Dtar020.DTAR020;

 	   AbstractLineReader reader  = iob.newReader("G:\\Users\\Bruce01\\RecordEditor_HSQL\\SampleFiles\\DTAR020.bin"); 
 	   

	   while ((line = reader.read()) != null) { 
		  // line.setFieldText(recordIdx, fieldIdx, value);
		  //  line.getFieldValue(dtar020.keycodeNo).set
		   IFieldValue fieldValue = line.getFieldValue(dtar020.keycodeNo);
		   setToSpaces1(line.getFieldValue(dtar020.keycodeNo));
			   
		   System.out.println(
				     "\t" + fieldValue.asString()
				   + "\t" + line.getFieldValue(dtar020.deptNo).asString()
				   + "\t" + line.getFieldValue(dtar020.qtySold).asString()
				   + "\t" + line.getFieldValue(dtar020.salePrice).asString()
				   );
	   }

	}

	private static void setToSpaces1(IFieldValue fieldValue) {
		IFieldDetail fieldDetail = fieldValue.getFieldDetail();
		int len = fieldDetail.getLen();
		String spaceHex = Conversion.isEbcidic(fieldDetail.getFontName())
			   ? "40"
			   : "20";
		StringBuilder b = new StringBuilder(len * 2 );
		for (int i = 0; i < len; i++) {
		   b.append(spaceHex);
		}
		fieldValue.setHex(b.toString());
	}


}
