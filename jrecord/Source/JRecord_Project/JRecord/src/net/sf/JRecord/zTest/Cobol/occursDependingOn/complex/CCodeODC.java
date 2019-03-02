package net.sf.JRecord.zTest.Cobol.occursDependingOn.complex;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.zTest.Cobol.occursDependingOn.Code;

public class CCodeODC {
	
	public static final String MONTHS_FIELD_NAME = Code.MONTHS_FIELD_NAME;
	public static final String WEEK_NO_FIELD_NAME = Code.WEEK_NO_FIELD_NAME;


	public static final int ODT_STANDARD_ARRAY = 1;
	public static final int ODT_NON_ARRAY_SIZE = 2;
	public static final int ODT_PARENT_ARRAY_SIZE = 3;
	public static final int ODT_ARRAY_SIZE = 4;
	
	public static String getCobol101(boolean occursDepending, int imbededArrayType, int monthCount, int weekNo, int purchCount) {
		String od1 = "occurs 1 to 12 depending on months";
		String od2 = "occurs 1 to 12 depending on week-no";
		String od11 = "occurs 1 to 5 depending on week-of-month1";
		String od21 = "occurs 1 to 5 depending on week-of-month2";
		
		if (! occursDepending) {
			od1 = "occurs " + monthCount;
			od2 = "occurs " + purchCount;
		}
		
		switch (imbededArrayType)  {
		case ODT_STANDARD_ARRAY:
			od11 = "occurs " + weekNo;
			od21 = od11;
			break;
		case ODT_NON_ARRAY_SIZE:
			od11 = "occurs 1 to 5 depending on week-of-month";
			od21 = od11;
			break;
		};
		
	
		String s = 
				   "        01 Location-Details.\n"
				 + "           03 Location-details.\n"
				 + "              05 Location-Number     Pic 9(4).\n"
				 + "              05 Location-Name       Pic X(25).\n"
				 + "           03 months                 Pic s99.\n"
				 + "           03 week-of-month          Pic s9.\n"
				 + "           03 days                   Pic s9.\n"
				 + "           03 filler\n"
				 + "              " + od1 + ".\n"
				 + "              05 week-of-month1      Pic s9.\n"
				 + "              05 " + od11 + ".\n"
				 + "                 15 daily-sales      Pic s9(7).\n"
				 + "              05 " + od11 + ".\n"
				 + "                 15 daily-value      Pic s9(7).\n"
				 + "              05 sales-count         Pic s9(7).\n"
				 + "              05 sales-value         Pic s9(9)v99.\n"
				 + "           03 total-sales            Pic s9(9)v99.\n"
				 + "           03 week-no                Pic s99.\n"
				 + "           03 filler\n"
				 + "              " + od2 + ".\n"
				 + "              05 purchase-count      Pic s9(7).\n"
				 + "              05 week-of-month2      Pic s9.\n"
				 + "              05 " + od21 + ".\n"
				 + "                 15 daily-purch-count  Pic s9(7).\n"
				 + "                 15 daily-purch-value  Pic s9(7).\n"
				 + "              05 purchase-value      Pic s9(9)v99.\n"
				 + "           03 total-purchase-count   Pic s9(9).\n"
				 + "           03 total-purchase-value   Pic s9(9)v99.\n";
		
//		System.out.println();
//		System.out.println();
//		System.out.println(s);
		
		return s;
		
	}
	
	public static String getCobol111(boolean occursDepending, int imbededArrayType1, int imbededArrayType2, 
			int monthCount, int weekNo, int purchCount, int days, int hours) {
		String od1 = "occurs 1 to 12 depending on months";
		String od2 = "occurs 1 to 12 depending on week-no";
		String od11   = "occurs 1 to 5 depending on week-of-month-1";
		String od111  = "occurs 1 to 4 depending on days-11";
		String od1111 = "occurs 1 to 5 depending on hours-11";
		String od121  = "occurs 1 to 4 depending on days-12";
		String od1211 = "occurs 1 to 5 depending on hours-12";
		String od211  = "occurs 1 to 4 depending on days-21";
		String od2111 = "occurs 1 to 5 depending on hours-21";
		String od21   = "occurs 1 to 5 depending on week-of-month-2";
		
		if (! occursDepending) {
			od1 = "occurs " + monthCount;
			od2 = "occurs " + purchCount;
		}
		
		switch (imbededArrayType1)  {
		case ODT_STANDARD_ARRAY:
			od11 = "occurs " + weekNo;
			od21 = od11;
			break;
		case ODT_NON_ARRAY_SIZE:
			od11 = "occurs 1 to 5 depending on week-of-month";
			od21 = od11;
			break;
		};

		switch (imbededArrayType2)  {
		case ODT_STANDARD_ARRAY:
			od111  = "occurs " + days;
			od121  = od111;
			od211  = od111;
			od1111 = "occurs " + hours;
			od1211 = od1111;
			od2111 = od1111;
			break;
		case ODT_NON_ARRAY_SIZE:
			od111  = "occurs 1 to 4 depending on days";
			od121  = od111;
			od211  = od111;
			od1111 = "occurs 1 to 5 depending on hours";
			od1211 = od1111;
			od2111 = od1111;
			break;
		case ODT_PARENT_ARRAY_SIZE:
			od111  = "occurs 1 to 4 depending on days-1";
			od121  = od111;
			od211  = "occurs 1 to 4 depending on days-2";
			od1111 = "occurs 1 to 5 depending on hours-1";
			od1211 = od1111;
			od2111 = "occurs 1 to 5 depending on hours-2";
			break;
		};

		String s
			 = "        01 Location-Details.\n"
			 + "           03 Location-details.\n"
			 + "              05 Location-Number     Pic 9(4).\n"
			 + "              05 Location-Name       Pic X(25).\n"
			 + "           03 months                 Pic s99.\n"
			 + "           03 week-of-month          Pic s9.\n"
			 + "           03 days                   Pic s9.\n"
			 + "           03 hours                  Pic s9.\n"
			 + "           03 filler\n"
			 + "              " + od1 + ".\n"
			 + "             05 week-of-month-1      Pic s9.\n"
			 + "             05 days-1               Pic s9.\n"
			 + "             05 hours-1              Pic s9.\n"
			 + "             05 " + od11 + ".\n"
			 + "                 10 week-sales       Pic s9(7).\n"
			 + "                 10 days-11          Pic s9.\n"
			 + "                 10 " + od111 + ".\n"
			 + "                    15 d-sales       Pic s9(7).\n"
			 + "                    15 hours-11      Pic s9.\n"
			 + "                    15 " + od1111 + ".\n"
			 + "                       20 daily-sales   Pic s9(7).\n"
			 + "              05 " + od11 + ".\n"
			 + "                 10 days-12          Pic s9.\n"
			 + "                 10 " + od121 + ".\n"
			 + "                    15 hours-12      Pic s9.\n"
			 + "                    15 " + od1211 + ".\n"
			 + "                       20 daily-value   Pic s9(7).\n"
			 + "                    15 d-value       Pic s9(7).\n"
			 + "                 10 week-value       Pic s9(7).\n"
			 + "              05 sales-count         Pic s9(7).\n"
			 + "              05 sales-value         Pic s9(9)v99.\n"
			 + "           03 total-sales            Pic s9(9)v99.\n"
			 + "           03 week-no                Pic s99.\n"
			 + "           03 filler\n"
			 + "              " + od2 + ".\n"
			 + "              05 week-of-month-2     Pic s9.\n"
			 + "              05 days-2              Pic s9.\n"
			 + "              05 hours-2             Pic s9.\n"
			 + "              05 "  + od21 + ".\n"
			 + "                 10 week-purch       Pic s9(7).\n"
			 + "                 10 days-21          Pic s9.\n"
			 + "                 10 " + od211 + ".\n"
			 + "                    15 d-purch       Pic s9(7).\n"
			 + "                    15 hours-21      Pic s9.\n"
			 + "                    15 " + od2111 + ".\n"
			 + "                       20 daily-purch     Pic s9(7).\n"
			 + "                       20 daily-purch-val Pic s9(7).\n"
			 + "                    15 d-purch-val   Pic s9(7).\n"
			 + "                 10 week-purch-val   Pic s9(7).\n"
			 + "              05 purchase-count      Pic s9(7).\n"
			 + "              05 purchase-value      Pic s9(9)v99.\n"
			 + "           03 total-purchase-count   Pic s9(9).\n"
			 + "           03 total-purchase-value   Pic s9(9)v99.\n";
		
//		System.out.println();
//		System.out.println();
//		System.out.println(s);

		return s;
	}
	
	public static int check(List<IFieldDetail> fieldList, AbstractLine line, IFieldDetail fld, int pos) throws RecordException {	
		return Code.check(fieldList, line, fld, pos);
	}
	
	public static void checkFieldIteratore(AbstractLine line, String tstId, ArrayList<IFieldDetail> l) {
		FieldIterator fi = line.getFieldIterator(0);
		int i = 0;

		while (fi.hasNext()) {
			AbstractFieldValue fv = fi.next();
			FieldDetail fieldDetail = (FieldDetail) fv.getFieldDetail();
			//System.out.print(fieldDetail.getName() + "\t");
			if (fieldDetail != l.get(i)) {
				TestCase.assertEquals(l.get(i).getName(), fieldDetail.getName() );
			}

			if (i == 10 && tstId.startsWith("0~2:1")) {
				System.out.print('*');
			}

			i += 1;
		}
	}
}
