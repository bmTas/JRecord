package net.sf.JRecord.zExamples.iob.cobol.iobuilder;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstRedefines02 {

	private static String copybook
				= "       01  Tst-Redef.\n"
				+ "           03  fld1                pic x(20).\n"
				+ "           03  fld2                pic 9(8).\n"
				+ "           03  redefines fld2.\n"
				+ "               05                  pic x.\n"
				+ "               05  fld-21          pic 9(3).\n"
				+ "               05  fld-22          pic 9(1).\n"
				+ "           03  fld3.\n"
				+ "               05  fld-31          pic x(10).\n"
				+ "               05  fld-32 occurs 20.\n"
				+ "                   07 fld-321      pic xx.\n"
				+ "                   07 fld-322      pic x(4).\n"
				+ "           03 fld4 redefines fld3.\n"
				+ "              05  fld-41           pic x.\n"
				+ "              05  fld-42           pic x(2).\n"
				+ "              05  fld-43 occurs 4.\n"
				+ "                  07 fld-431       pic x(5).\n"
				+ "                  07 fld-432       pic 9(5).\n"
				+ "           03 fld5.\n"
				+ "              05  fld-51           pic x.\n"
				+ "              05  fld-52           pic x(2).\n"
				+ "              05  fld-53 occurs 4.\n"
				+ "                  07 fld-531       pic x(5).\n"
				+ "                  07 fld-532       pic 9(5).\n";
	
	public static void main(String[] args) throws IOException {
		ICobolIOBuilder iob =JRecordInterface1.COBOL.newIOBuilder(new StringReader(copybook), "Tst-Redef");
		
		LayoutDetail l = iob.getLayout();
		RecordDetail rec = l.getRecord(0);
		FieldDetail[] flds = new FieldDetail[rec.getFieldCount()];
//		TreeMap<String, FieldDetail> dups = new TreeMap<String, FieldDetail>();
		ArrayList<FieldDetail> nonRedef = new ArrayList<FieldDetail>();
		
		for (int i = 0; i < flds.length; i++) {
			flds[i] = rec.getField(i);
		}
		
		Arrays.sort(flds, new Comparator<FieldDetail>() {
			@Override public int compare(FieldDetail o1, FieldDetail o2) {
				int ret = 1;
				if (o1.getPos() < o2.getPos()) {
					ret = -1;
				} else if (o1.getPos() == o2.getPos()) {
					ret = 0;
					if (o1.getEnd() > o2.getEnd()) {
						ret = -1;
					}
				}
				return ret;
			}
		});
		
		FieldDetail last = flds[0];
		boolean toPrint = true;
		
		for (int i = 1; i < flds.length; i++) {
			if (last.getEnd() >= flds[i].getPos()) {
				toPrint = false;
				if (last.getEnd() < flds[i].getEnd()) {
					last = flds[i];
				}
			} else { 
				if (toPrint) {
					System.out.println(last.getLookupName() + "\t" + last.getPos() + "\t" + last.getLen());
					nonRedef.add(last);
				}
				toPrint = true;
				last = flds[i];
			}
		}
		if (toPrint) {
			System.out.println(last.getLookupName() + "\t" + last.getPos() + "\t" + last.getLen());
			nonRedef.add(last);
		}
		
//		Collection<FieldDetail> values = dups.values();
//		
//		for (FieldDetail v : values) {
//			System.out.println(v.getLookupName() + "\t" + v.getPos() + "\t" + v.getLen());
//		}
	}

}
