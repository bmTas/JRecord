package net.sf.JRecord.zTest.cgen;

import java.io.IOException;
import java.time.LocalDate;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.cgen.def.IFieldSerDes;
import net.sf.JRecord.cgen.support.DateFieldSerDes;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;

public class TstDateDerSer extends TestCase {

	private static DateData[] dates = {
			new DateData(1999, 02, 11),
			new DateData(2000, 01, 01),
			new DateData(2000, 01, 02),
			new DateData(2001, 02, 02),
			new DateData(2002, 03, 04),
			new DateData(2011, 12, 12),
			new DateData(2014, 12, 16),
			new DateData(2017, 02, 18),
	};
	
	public void test01() throws IOException {
		IFixedWidthIOBuilder iob = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.defineFieldsByLength()
						.addFieldByLength("Field1", Type.ftBinaryBigEndian, 4, 0)
						.addFieldByLength("Field2", Type.ftChar, 15, 0)
					.endOfRecord();
		AbstractLine line = iob.newLine(new byte[] {0,0,0,0});
		IFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldValue fieldValueChar = line.getFieldValue(0, 1);
		for (DateData dd : dates) {
			tstDates(fieldValue, fieldValueChar, dd);
		}
	}
	
	public void test02() throws IOException {
		IFixedWidthIOBuilder iob = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.defineFieldsByLength()
						.addFieldByLength("Field1", Type.ftBinaryBigEndian, 4, 0)
					.endOfRecord();
		AbstractLine line = iob.newLine(new byte[] {0,0,0,0});
		IFieldValue fieldValue = line.getFieldValue(0, 0);
		for (DateData dd : dates) {
			fieldValue.set(dd.ddmmyy);
			System.out.println(dd.date + " " + DateFieldSerDes.getDDMMYY().getFromField(fieldValue) + " " + dd.ddmmyy);
		}
	}
	
	
	private void tstDates(IFieldValue fv, IFieldValue fvChar, DateData dd ) {
		tst1date(fv, DateFieldSerDes.YYYYMMDD, dd.date, dd.yyyymmdd);
		tst1date(fv, DateFieldSerDes.CYYMMDD, dd.date, dd.cyymmdd);
		tst1date(fv, DateFieldSerDes.DDMMYYYY, dd.date, dd.ddmmyyyy);
		tst1date(fv, DateFieldSerDes.MMDDYYYY, dd.date, dd.mmddyyyy);
		tst1date(fv, DateFieldSerDes.YYMMDD, dd.date, dd.yymmdd);
		tst1date(fv, DateFieldSerDes.getDDMMYY(), dd.date, dd.ddmmyy);
		
		char[] seps = {'/', '-', '.', ' '};
		for (char sep : seps ) {
			tst1StrDate(fvChar, DateFieldSerDes.getDD_MM_YY(sep), dd.date,
					dd.ddStr + sep + dd.mmStr + sep + dd.yy2Str, dd.dd2Str + sep + dd.mm2Str + sep + dd.yy2Str);
			tst1StrDate(fvChar, DateFieldSerDes.getMM_DD_YY(sep), dd.date,
					dd.mmStr + sep + dd.ddStr + sep + dd.yy2Str, dd.mm2Str + sep + dd.dd2Str + sep + dd.yy2Str);
			tst1StrDate(fvChar, DateFieldSerDes.getYY_MM_DD(sep), dd.date,
					dd.yy2Str + sep + dd.mmStr + sep + dd.ddStr, dd.yy2Str + sep + dd.mm2Str + sep + dd.dd2Str);

			tst1StrDate(fvChar, DateFieldSerDes.getDD_MM_YYYY(sep), dd.date,
					dd.ddStr + sep + dd.mmStr + sep + dd.yyyyStr, dd.dd2Str + sep + dd.mm2Str + sep + dd.yyyyStr);
			tst1StrDate(fvChar, DateFieldSerDes.getMM_DD_YYYY(sep), dd.date,
					dd.mmStr + sep + dd.ddStr + sep + dd.yyyyStr, dd.mm2Str + sep + dd.dd2Str + sep + dd.yyyyStr);
			tst1StrDate(fvChar, DateFieldSerDes.getYYYY_MM_DD(sep), dd.date,
					dd.yyyyStr + sep + dd.mmStr + sep + dd.ddStr, dd.yyyyStr + sep + dd.mm2Str + sep + dd.dd2Str);
		}
		//char[] seps1 = {' ', '/', '-', '.'};
		for (char sep : seps ) {
			tst1StrDate(fvChar, DateFieldSerDes.getDD_Mth_YY(sep), dd.date,
					dd.ddStr + sep + dd.MthStr + sep + dd.yy2Str, dd.dd2Str + sep + dd.MthStr + sep + dd.yy2Str);
			tst1StrDate(fvChar, DateFieldSerDes.getMth_DD_YY(sep), dd.date,
					dd.MthStr + sep + dd.ddStr + sep + dd.yy2Str, dd.MthStr + sep + dd.dd2Str + sep + dd.yy2Str);
			tst1StrDate(fvChar, DateFieldSerDes.getYY_Mth_DD(sep), dd.date,
					dd.yy2Str + sep + dd.MthStr + sep + dd.ddStr, dd.yy2Str + sep + dd.MthStr + sep + dd.dd2Str);

			tst1StrDate(fvChar, DateFieldSerDes.getDD_Mth_YYYY(sep), dd.date,
					dd.ddStr + sep + dd.MthStr + sep + dd.yyyyStr, dd.dd2Str + sep + dd.MthStr + sep + dd.yyyyStr);
			tst1StrDate(fvChar, DateFieldSerDes.getMth_DD_YYYY(sep), dd.date,
					dd.MthStr + sep + dd.ddStr + sep + dd.yyyyStr, dd.MthStr + sep + dd.dd2Str + sep + dd.yyyyStr);
			tst1StrDate(fvChar, DateFieldSerDes.getYYYY_Mth_DD(sep), dd.date,
					dd.yyyyStr + sep + dd.MthStr + sep + dd.ddStr, dd.yyyyStr + sep + dd.MthStr + sep + dd.dd2Str);

		}
	}
	
	private void tst1date(IFieldValue fv, IFieldSerDes<LocalDate> derSer, LocalDate date, int intVal) {
		fv.set(intVal);
		assertEquals(date, derSer.getFromField(fv));
		fv.set(0);
		derSer.setField(fv, date);
		assertEquals(intVal, fv.asInt());
		
	}
	
	private void tst1StrDate(IFieldValue fv, IFieldSerDes<LocalDate> derSer, LocalDate date, String strVal1, String strVal2) {
		fv.set(strVal1);
		assertEquals(date, derSer.getFromField(fv));
		fv.set(strVal2);
		assertEquals(date, derSer.getFromField(fv));
		fv.set(0);
		derSer.setField(fv, date);
		assertEquals(strVal1, fv.asString());
		
	}
	
	private static class DateData {
		public final LocalDate date;
		public final int yymmdd, yyyymmdd, cyymmdd, ddmmyy, ddmmyyyy, mmddyyyy;
		public final String ddStr, mmStr, dd2Str, mm2Str, yy2Str, yyyyStr, MthStr;


		public DateData(int yyyy, int mm, int dd) {
			String[] months = {
					"Jan", "Feb", "Mar", "Apr",
					"May", "Jun", "Jul", "Aug",
					"Sep", "Oct", "Nov", "Dec"
			};
			int yy = yyyy % 100;
			
			this.ddStr = Integer.toString(dd);
			this.mmStr = Integer.toString(mm);
			String yyStr = Integer.toString(yy);
			this.yyyyStr = Integer.toString(yyyy);
			
			this.dd2Str = ddStr.length() == 1 ? "0" + ddStr : ddStr;
			this.mm2Str = mmStr.length() == 1 ? "0" + mmStr : mmStr;
			this.yy2Str = yyStr.length() == 1 ? "0" + yyStr : yyStr;
			
			this.date = LocalDate.of(yyyy, mm, dd);
			this.yyyymmdd = yyyy * 10000 + mm * 100 + dd;
			
			this.yymmdd = yyyymmdd % 1000000;
			this.cyymmdd = yyyymmdd - 19000000;
			

			this.ddmmyyyy = yyyy + mm * 10000 + dd * 1000000;
			this.ddmmyy = yy + mm * 100 + dd * 10000;
			
			this.mmddyyyy = yyyy  + mm * 1000000 + dd * 10000;
			
			this.MthStr = months[mm-1];
		}
	}
}
