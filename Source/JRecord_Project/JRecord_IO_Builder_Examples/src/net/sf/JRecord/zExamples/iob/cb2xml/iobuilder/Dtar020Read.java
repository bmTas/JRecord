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
      
package net.sf.JRecord.zExamples.iob.cb2xml.iobuilder;

import java.io.FileNotFoundException;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.Icb2xmlIOBuilder;

public class Dtar020Read {

	public static void main(String[] a) throws FileNotFoundException, IOException {
	
	   Icb2xmlIOBuilder iob = JRecordInterface1.CB2XML
		        .newIOBuilder("G:\\Users\\BruceTst01\\RecordEditor_HSQL\\CopyBook\\cb2xml\\DTAR020.xml")
		             .setFont("cp037");
    
	   AbstractLine line;
	   ClassDtar020 dtar020 = DTAR020;

 	   AbstractLineReader reader  = iob.newReader("G:\\Users\\BruceTst01\\RecordEditor_HSQL\\SampleFiles\\DTAR020.bin"); 
 	   

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
	   reader.close();
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

    public static final ClassDtar020 DTAR020 = new ClassDtar020();


    public static final class ClassDtar020 { 

        public final String recordName = "DTAR020";

        public final String keycodeNo = "DTAR020-KEYCODE-NO";
        public final String storeNo = "DTAR020-STORE-NO";
        public final String date = "DTAR020-DATE";
        public final String deptNo = "DTAR020-DEPT-NO";
        public final String qtySold = "DTAR020-QTY-SOLD";
        public final String salePrice = "DTAR020-SALE-PRICE";

    }

}
