package net.sf.JRecord.cgen.support;

import java.time.LocalDate;
import java.time.Year;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.cgen.def.IFieldSerDes;

public class DateFieldSerDes implements IFieldSerDes<LocalDate> {
	
	private static final int idx_yy_m_d = 0;
	private static final int idx_y_m_d = 1;
	private static final int idx_d_m_yy = 2;
	private static final int idx_d_m_y = 3;
	private static final int idx_m_d_yy = 4;
	private static final int idx_m_d_y = 5;

	private static final int idx_yy_mth_d = 6;
	private static final int idx_y_mth_d = 7;
	private static final int idx_d_mth_yy = 8;
	private static final int idx_d_mth_y = 9;
	private static final int idx_mth_d_yy = 10;
	private static final int idx_mth_d_y = 11;

	private static final int YEAR_VALUE = Year.now().getValue();
	private static final int CENTURARY_WINDOW_YEAR = YEAR_VALUE - 80;

	private static final DateFieldSerDes[] dateCnv = new DateFieldSerDes[ (idx_mth_d_y + 1) * 4];
	public static final IFieldSerDes<LocalDate> DDMMYYYY = new DMYYConverter(),
			                                    YYYYMMDD = new YYMDConverter(0),
			                                    CYYMMDD  = new YYMDConverter(19000000),
			                                    MMDDYYYY = new MDYYConverter(),
			                                    YYMMDD   = new YMDConverter();
	
	private static DateFieldSerDes ddmmyy;
	
	
	public static DateFieldSerDes getDDMMYY() {
		if (ddmmyy == null) {
			ddmmyy = getDMYconversion("ddMM", 6);
		}
		return ddmmyy;
	}



	/**
	 * return Create a Date Serialiser
	 * @param sep date separator field
	 * @return
	 */
	public static DateFieldSerDes getYYYY_MM_DD(char sep) {
		return getDateSerDes(getIdx(idx_yy_m_d, sep), "yyyy" + sep + "M" + sep + "d");
	}
	public static DateFieldSerDes getYY_MM_DD(char sep) {
		return getDateSerDesYYstart(getIdx(idx_y_m_d, sep), sep + "M" + sep + "d");
	}

	public static DateFieldSerDes getYYYY_Mth_DD(char sep) {
		return getDateSerDes(getIdx(idx_yy_mth_d, sep), "yyyy" + sep + "MMM" + sep + "d");
	}
	public static DateFieldSerDes getYY_Mth_DD(char sep) {
		return getDateSerDesYYstart(getIdx(idx_y_mth_d, sep), sep + "MMM" + sep + "d");
	}
	public static DateFieldSerDes getDD_MM_YYYY(char sep) {
		return getDateSerDes(getIdx(idx_d_m_yy, sep), "d" + sep + "M" + sep + "yyyy");
	}

	public static DateFieldSerDes getDD_MM_YY(char sep) {
		return getDateSerDesYYend(getIdx(idx_d_m_y, sep), "d" + sep + "M"  + sep);
	}

	public static DateFieldSerDes getDD_Mth_YYYY(char sep) {
		return getDateSerDes(getIdx(idx_d_mth_yy, sep), "d" + sep + "MMM" + sep + "yyyy");
	}

	public static DateFieldSerDes getDD_Mth_YY(char sep) {
		return getDateSerDesYYend(getIdx(idx_d_mth_y, sep), "d" + sep + "MMM"  + sep);
	}

	public static DateFieldSerDes getMM_DD_YYYY(char sep) {
		return getDateSerDes(getIdx(idx_m_d_yy, sep), "M" + sep + "d" + sep + "yyyy");
	}

	public static DateFieldSerDes getMM_DD_YY(char sep) {
		return getDateSerDesYYend(getIdx(idx_m_d_y, sep), "M" + sep + "d"  + sep);
	}

	public static DateFieldSerDes getMth_DD_YYYY(char sep) {
		return getDateSerDes(getIdx(idx_mth_d_yy, sep), "MMM" + sep + "d" + sep + "yyyy");
	}

	public static DateFieldSerDes getMth_DD_YY(char sep) {
		return getDateSerDesYYend(getIdx(idx_mth_d_y, sep), "MMM" + sep + "d"  + sep);
	}

	private static DateFieldSerDes getDateSerDes(int idx, String pattern) {
		DateFieldSerDes ds = idx < 0 ? null : dateCnv[idx];
		if (ds == null) {
			ds = new DateFieldSerDes(DateTimeFormatter.ofPattern(pattern), 0);
			if (idx >= 0) {
				dateCnv[idx] = ds;
			}
		}

		return ds;
	}


	private static DateFieldSerDes getDateSerDesYYend(int idx, String pattern) {
		DateFieldSerDes ds = idx < 0 ? null : dateCnv[idx];
		if (ds == null) {
			ds = getDMYconversion(pattern, 0);
			if (idx >= 0) {
				dateCnv[idx] = ds;
			}
		}

		return ds;
	}

	private static DateFieldSerDes getDateSerDesYYstart(int idx, String pattern) {
		DateFieldSerDes ds = idx < 0 ? null : dateCnv[idx];
		if (ds == null) {
			ds = new DateFieldSerDes(
					new DateTimeFormatterBuilder()
						.appendValueReduced(ChronoField.YEAR, 2, 2, CENTURARY_WINDOW_YEAR)
						.appendPattern(pattern)
					.toFormatter(), 
					0);
			if (idx >= 0) {
				dateCnv[idx] = ds;
			}
		}

		return ds;
	}
	
	private static DateFieldSerDes getDMYconversion(String ddmmPattern, int len) {
		return new DateFieldSerDes(
					new DateTimeFormatterBuilder()
						.appendPattern(ddmmPattern)
						.appendValueReduced(ChronoField.YEAR, 2, 2, CENTURARY_WINDOW_YEAR)
					.toFormatter(), 
					len);
	}


	private static int getIdx(int base, char sep) {
		int idx = base * 4;
		switch (sep ) {
		case '/': break;
		case '-': idx += 1; break;
		case '.': idx += 2; break;
		case ' ': idx += 3; break;
		default:
			idx = -1;
		}
		
		return idx;
	}
	private static String ZEROS = "000000000000000000000000000000000000000000000000000000000";

	public DateFieldSerDes(DateTimeFormatter formatter, int length) {
		super();
		this.formatter = formatter;
		this.length = length;
	}
	
	
	private final DateTimeFormatter formatter;
	private final int length;
	
	
	public LocalDate getFromField(AbstractFieldValue dateFld) {
		String date = dateFld.asString();
		return LocalDate.parse(date.length() >= length ? date : ZEROS.substring(0, length-date.length()) + date , formatter);
	}
	public void setField(AbstractFieldValue dateFld, LocalDate date) {
		dateFld.set(formatter.format(date));
	}

	
	private static class DMYYConverter implements IFieldSerDes<LocalDate> {

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#getFromField(net.sf.JRecord.Details.fieldValue.IFieldValue)
		 */
		@Override
		public LocalDate getFromField(AbstractFieldValue dateFld) {
			int dmyy = dateFld.asInt();
			return LocalDate.of(dmyy % 10000, (dmyy / 10000) % 100, dmyy / 1000000);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#setField(net.sf.JRecord.Details.fieldValue.IFieldValue, java.lang.Object)
		 */
		@Override
		public void setField(AbstractFieldValue dateFld, LocalDate d) {
			dateFld.set(d.getDayOfMonth() * 1000000 + d.getMonthValue() * 10000 + d.getYear());
		}
	}
	
	
	private static class YYMDConverter implements IFieldSerDes<LocalDate> {
		private final int adj;
		
		public YYMDConverter(int adj) {
			super();
			this.adj = adj;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#getFromField(net.sf.JRecord.Details.fieldValue.IFieldValue)
		 */
		@Override
		public LocalDate getFromField(AbstractFieldValue dateFld) {
			int yymd = dateFld.asInt() + adj;
			return LocalDate.of(yymd / 10000, (yymd / 100) % 100, yymd % 100);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#setField(net.sf.JRecord.Details.fieldValue.IFieldValue, java.lang.Object)
		 */
		@Override
		public void setField(AbstractFieldValue dateFld, LocalDate d) {
			dateFld.set(d.getYear() * 10000 + d.getMonthValue() * 100 + d.getDayOfMonth() - adj);
		}
	}
	
	
	private static class MDYYConverter implements IFieldSerDes<LocalDate> {

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#getFromField(net.sf.JRecord.Details.fieldValue.IFieldValue)
		 */
		@Override
		public LocalDate getFromField(AbstractFieldValue dateFld) {
			int mdyy = dateFld.asInt();
			return LocalDate.of(mdyy % 10000, mdyy / 1000000, (mdyy / 10000) % 100);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#setField(net.sf.JRecord.Details.fieldValue.IFieldValue, java.lang.Object)
		 */
		@Override
		public void setField(AbstractFieldValue dateFld, LocalDate d) {
			dateFld.set(d.getYear()  + d.getMonthValue() * 1000000 + d.getDayOfMonth() * 10000);
		}
	}
	
	
	private static class YMDConverter implements IFieldSerDes<LocalDate> {
		private final int yyNow = YEAR_VALUE % 100;
		private final int yyWindow = CENTURARY_WINDOW_YEAR % 100;
		private final int cc = YEAR_VALUE / 100;
		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#getFromField(net.sf.JRecord.Details.fieldValue.IFieldValue)
		 */
		@Override
		public LocalDate getFromField(AbstractFieldValue dateFld) {
			int ymd = dateFld.asInt();
			int yymd, yy = ymd / 10000;
			if (yyNow >= 80) {
				yymd = (yy <  yyWindow ? cc + 1 : cc) * 1000000 + ymd;
			} else {
				yymd = (yy >= yyWindow ? cc - 1 : cc) * 1000000 + ymd;
			}
			return LocalDate.of(yymd / 10000, (yymd / 100) % 100, yymd % 100);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.cgen.defJr.IFieldFormatter#setField(net.sf.JRecord.Details.fieldValue.IFieldValue, java.lang.Object)
		 */
		@Override
		public void setField(AbstractFieldValue dateFld, LocalDate d) {
			dateFld.set(d.getYear() % 100 * 10000 + d.getMonthValue() * 100 + d.getDayOfMonth());
		}
	}

}
