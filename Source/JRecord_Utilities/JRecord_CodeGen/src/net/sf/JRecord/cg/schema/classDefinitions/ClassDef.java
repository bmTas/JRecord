package net.sf.JRecord.cg.schema.classDefinitions;

import java.util.HashMap;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.IRecordEditorFormat;

public abstract class ClassDef implements IClassDef {
	
	private static HashMap<String, DateFormatStr> dateFormatToName = new HashMap<String, DateFormatStr>(100);
	
	static {
		addOpts("yy", "MM", "dd");
		addOpts("dd", "MM", "yy");
		addOpts("MM", "dd", "yy");
		addOpts("yyyy", "MM", "dd");
		addOpts("dd", "MM", "yyyy");
		addOpts("MM", "dd", "yyyy");
		
		addDateFormatStr("yy MMM dd", "YY_MMM_DD", " ");
		addDateFormatStr("yyyy MMM dd", "YYYY_MMM_DD", " ");
		addDateFormatStr("dd MMM yy", "DD_MMM_YY", " ");
		addDateFormatStr("dd MMM yyyy", "DD_MMM_YYYY", " ");
	}
	
	private static void addOpts(String p1, String p2, String p3) {

		String p = p1 + p2 + p3;
		addDateFormatStr(p, p.toUpperCase(), "");
		
		String s = (p1 + "_" + p2 + "_" + p3).toUpperCase();
		addDateFormatStr(p1 +"/" + p2 + "/" + p3, s, "/");
		addDateFormatStr(p1 +"-" + p2 + "-" + p3, s + "1", "-");
		addDateFormatStr(p1 +"." + p2 + "." + p3, s + "2", ".");
		addDateFormatStr(p1 +" " + p2 + " " + p3, s + "3", " ");
	}
	
	
	private static void addDateFormatStr(String format, String name, String sep) {
		//System.out.println(format + "\t~\t" + name);
		dateFormatToName.put(format, new DateFormatStr(format, name, sep));
	}
	
	public static ClassDef getClassDef(IFieldDetail fieldDetail) {
		return getClassDef(fieldDetail.getFormat(), fieldDetail.getParamater());
	}
	
	public static ClassDef getClassDef(int format, String param) {
		switch (format) {
		case IRecordEditorFormat.FMT_DATE_DMYY: return new ClassDefDMYY();
		case IRecordEditorFormat.FMT_DATE_YYMD: return new ClassDefYYMD();
		case IRecordEditorFormat.FMT_DATE_CYMD: return new ClassDefYYMD("19000000");
		case IRecordEditorFormat.FMT_DATE:
			DateFormatStr formatDef;
			if (param == null || param.length() == 0) {
				return null;
			} else if ("yyyyMMdd".equals(param)) {
				return new ClassDefYYMD();
			} else if ("ddMMyyyy".equals(param)) {
				return new ClassDefDMYY();
			} else if ("MMddyyyy".equals(param)) {
				return new ClassDefMDYY();
			} else if ((formatDef = dateFormatToName.get(param)) != null) {
				return new ClassDate(formatDef);
			}
		default:
			return null;
		}
	}

	public final String dataImport, className, call, code, jrecAs;
	private final String[] conversionImport;

	public ClassDef(String dataImport, String className, String jrecAs, String[] conversionImport, String call, String code) {
		super();
		this.dataImport = dataImport;
		this.className = className;
		this.jrecAs = jrecAs;
		this.conversionImport = conversionImport;
		this.call = call;
		this.code = code;
	}

	/**
	 * @return the dataImport
	 */
	@Override
	public final String getDataImport() {
		return dataImport;
	}

	/**
	 * @return the className
	 */
	@Override
	public final String getClassName() {
		return className;
	}

	/**
	 * @return the jrecAs
	 */
	@Override
	public final String getJrecAs() {
		return jrecAs;
	}

	/**
	 * @return the conversionImport
	 */
	@Override
	public final String[] getConversionImport() {
		return conversionImport;
	}
	
	
	@Override
	public String generateToPojo(String variable) {
		return variable;
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.schema.IClassDef#generateFromPojo(java.lang.String)
	 */
	@Override
	public String generateFromPojo(String variable) {
		return variable;
	}

	@Override
	public String generateCall(int typeId, String variable) {
		return call + "(" + variable + ");";
	}

	/**
	 * @return the code
	 */
	@Override
	public final String getCode() {
		return code;
	}
	

}
