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

public class TstRedefines03 {

	private static enum FieldType {
		NORMAL, REDEFINED, REDEFINES
	};
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
				+ "           03 fld5 redefines fld3.\n"
				+ "              05  fld-51           pic x.\n"
				+ "              05  fld-52           pic x(2).\n"
				+ "              05  fld-53 occurs 5.\n"
				+ "                  07 fld-531       pic x(5).\n"
				+ "                  07 fld-532       pic 9(5).\n"
				+ "           03 fld6.\n"
				+ "              05  fld-61           pic x.\n"
				+ "              05  fld-62           pic x(2).\n"
				+ "              05  fld-63 occurs 4.\n"
				+ "                  07 fld-631       pic x(5).\n"
				+ "                  07 fld-632       pic 9(5).\n";
	
	public static void main(String[] args) throws IOException {
		ICobolIOBuilder iob =JRecordInterface1.COBOL.newIOBuilder(new StringReader(copybook), "Tst-Redef");
		
		LayoutDetail l = iob.getLayout();
		RecordDetail rec = l.getRecord(0);
		FieldDtls[] flds = new FieldDtls[rec.getFieldCount()];
//		TreeMap<String, FieldDetail> dups = new TreeMap<String, FieldDetail>();
		ArrayList<FieldDtls> normalFields = new ArrayList<FieldDtls>();
		ArrayList<FieldDtls> redef = new ArrayList<FieldDtls>();
		
		for (int i = 0; i < flds.length; i++) {
			flds[i] = new FieldDtls(rec.getField(i) , i);
		}

		// Sort into position // array position sequence
		Arrays.sort(flds, new Comparator<FieldDtls>() {
			@Override public int compare(FieldDtls o1, FieldDtls o2) {
				int ret = 1;
				int pos1 = o1.getPos();
				int pos2 = o2.getPos();
				if (pos1 < pos2) {
					ret = -1;
				} else if (pos1 == pos2 && o1.initialPosition < o2.initialPosition ) {
						ret = -1;
				}
				return ret;
			}
		});
		
		FieldDtls last = flds[0];
		boolean toPrint = true;
		
		for (int i = 1; i < flds.length; i++) {
			if (last.getEnd() >= flds[i].getPos()) {
				toPrint = false;
				if (last.toSave) {
					last.type = getFieldType(last, flds[i]);
					last.toSave = false;
					redef.add(last);
				}
				flds[i].type = getFieldType(flds[i], last);
				flds[i].toSave = false;
				redef.add(flds[i]);
				if (last.getEnd() < flds[i].getEnd()) {
					last = flds[i];
				}
			} else { 
				if (toPrint) {
					normalFields.add(last);
					last.toSave = false;
				}
				toPrint = true;
				last = flds[i];
			}
		}
		if (toPrint) {
			normalFields.add(last);
		}
		
		System.out.println();
		System.out.println("    Normal Fields");
		System.out.println();
		
		filterPrint(normalFields, FieldType.NORMAL);
		
		System.out.println();
		System.out.println("    Redefined");
		System.out.println();
		
		filterPrint(redef, FieldType.REDEFINED);
		
		System.out.println();
		System.out.println("    Redefines");
		System.out.println();
		
		filterPrint(redef, FieldType.REDEFINES);
	}

	static FieldType getFieldType(FieldDtls f1, FieldDtls f2) {
		return f1.initialPosition < f2.initialPosition ? FieldType.REDEFINED : FieldType.REDEFINES; 
	}
	
	static void filterPrint(ArrayList<FieldDtls> list, FieldType type) {
		for (FieldDtls item : list) {
			if (item.type == type) {
				System.out.println(item.fld.getLookupName() + "\t" + item.getPos() + "\t" + item.fld.getLen());				
			}
		}
	}
	private static class FieldDtls {
		boolean toSave = true;
		FieldType type = FieldType.NORMAL;
		final FieldDetail fld;
		final int initialPosition;
		
		protected FieldDtls(FieldDetail fld, int initialPosition) {
			super();
			this.fld = fld;
			this.initialPosition = initialPosition;
		}

		/**
		 * @return
		 * @see net.sf.JRecord.Common.FieldDetail#getPos()
		 */
		public int getPos() {
			return fld.getPos();
		}

		/**
		 * @return
		 * @see net.sf.JRecord.Common.FieldDetail#getEnd()
		 */
		public int getEnd() {
			return fld.getEnd();
		}
		
	}
}
