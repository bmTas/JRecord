package net.sf.JRecord.cbl2xml.zTest.xml2cbl.cobol;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Code;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CreateArrayDependFile {

	public CreateArrayDependFile() throws IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
						.newIOBuilder(Code.getFullName("cobol/ArrayDependingCopybook.cbl"))
								.setDialect(ICopybookDialects.FMT_FUJITSU)
								.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
						;
		BufferedWriter w = new BufferedWriter( new FileWriter("G:\\temp\\ArrayDependingFile.txt"));
		
		for (int i = 0; i < 20; i++) {
			AbstractLine l = ioBldr.newLine();
			long v1 = ((long) (i)) * 100000000;
			int g3 = Math.min(7, Math.max(1, i));
			int g6 = Math.min(6, Math.max(1, i - 2));
			int g8 = Math.min(5, Math.max(1, i - 4));
			l.getFieldValue( "field-1" ).set(v1 + 1);
			l.getFieldValue( "field-2" ).set(v1 + 2);
			l.getFieldValue( "group-3-count" ).set(g3);
			l.getFieldValue( "group-6-count" ).set(g6);
			l.getFieldValue( "group-8-count" ).set(g8);
			for (int j = 0; j < g3; j++) {
				long v2 = v1 + j * 1000000;
				l.getFieldValue( "field-4 (" + j + ")" ).set((long)v2 + 4);
				l.getFieldValue( "field-5 (" + j + ")").set((long) (v2 + 5));
				for (int k = 0; k < g6; k++) {
					long v3 = v2 + k * 10000;
					l.getFieldValue( "field-7 (" + j + ", " + k + ")" ).set(v3 + 7);
					for (int i1 = 0; i1 < g8; i1++) {
						long v4 = v3 + i1 * 100;
						l.getFieldValue( "field-a (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 8);
						l.getFieldValue( "field-b (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 9);
						l.getFieldValue( "field-c (" + j + ", " + k  + ", " + i1 + ")" ).set(v4 + 3);
					}
				}
			}
			w.write(l.getFullLine());
			w.newLine();
		}
		w.close();
		
	}
	
	public static void main(String[] args) throws IOException {
		new CreateArrayDependFile();
	}
}
