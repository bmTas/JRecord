package net.sf.JRecord.constantNames;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import net.sf.JRecord.Common.Constants;

public class ConstantNameConversion implements IInterfaceDetails {

	private static final int MAX_ARRAY_SIZE = 600;

	private final String simpleName, fullName;
	
	private final ConstantDetails[] codeLookup;
	private final HashMap<Integer, ConstantDetails> codeMap;
	
	private final HashMap<String, ConstantDetails> nameLookup;
	
	private final List<ConstantDetails> constantList;
	
	private final int maxCode;
	
	
	public ConstantNameConversion(Class<?> constantClass, ConstantDetails[] codes) {
		this(constantClass.getSimpleName(), constantClass.getName(), codes);
	}
	
	public ConstantNameConversion(String simpleName, String fullName, ConstantDetails[] codes) {
		super();
		this.simpleName = simpleName;
		this.fullName = fullName;
		this.nameLookup = new HashMap<>(5 * codes.length);
		this.constantList = Arrays.asList(codes);
		
		int max = 0;
		boolean greaterThanZero = true;
		for (ConstantDetails details : codes) {
			details.setParentInterfaceDetails(this);
			max = Math.max(max, details.getCode());
			if ( details.getCode() < 0) {
				greaterThanZero = false;
			}
		}
		maxCode = max;
		
		if (greaterThanZero && max < MAX_ARRAY_SIZE) {
			codeLookup = new ConstantDetails[max+1];
			codeMap = null;
		} else {
			codeLookup = new ConstantDetails[MAX_ARRAY_SIZE];
			codeMap = new HashMap<>();
		}
		
		for (ConstantDetails details : codes) {
			int code = details.getCode();
			if (code >= 0 && code < codeLookup.length) {
				codeLookup[code] = details;
			} else {
				codeMap.put(code, details);
			}
			addNamesToMap(details, details.getExternalName(), details.getJRecordConstantNoClassName(), details.getSimpleName());
			addNamesToMap(details, details.getExtraNames());
			//nameLookup.put(Integer.toString(details.getCode()), details);
		}

	//	this.codeLookup = codeLookup;
	}
	
	private void addNamesToMap(ConstantDetails constant, String... names) {
		if (names != null) {
			String last = null;
			for (String name : names) {
				if (name != null && name.length() > 0 && name != last) {
					nameLookup.put(name.toLowerCase(), constant);
					last = name;
				}
			}
		}
	}
	
	/**
	 * @return the constantList
	 */
	public List<ConstantDetails> getConstantList() {
		return constantList;
	}

	/**
	 * @return the maxCode
	 */
	public int getMaxCode() {
		return maxCode;
	}

	/**
	 * Get the constant details for a code;
	 * @param code constant code number
	 * @return constant code/name details
	 */
	public ConstantDetails getConstantDetails(int code) {
		if (code >= 0 && code < codeLookup.length) {
			return codeLookup[code];
		}
		return codeMap == null ? null : codeMap.get(code);
	}
	
	public String getConstantName(int code) {
		ConstantDetails cd = getConstantDetails(code);
		if (cd != null) {
			return cd.getSimpleName();
		}
		return Integer.toString(code);
	}
	
	/**
	 * Get the JRecord Constant as a String from the integer code
	 * @param code code value
	 * @return requested constant string
	 */
	public String getJRecordConstantName(int code) {
		ConstantDetails cd = getConstantDetails(code);
		if (cd != null) {
			return simpleName + "." + cd.getJRecordConstantNoClassName();
		}
		return Integer.toString(code);
	}
	
	/**
	 * Get the constant details for a code;
	 * @param code constant code number
	 * @return constant code/name details
	 */
	public ConstantDetails getConstantDetails(String name) {
		if (name == null) { return null;}
		
		ConstantDetails constantDetails = nameLookup.get(name.toLowerCase());
		if (constantDetails == null && name.length() < 4) {
			try {
				int code = Integer.parseInt(name);
				constantDetails = getConstantDetails(code);
			} catch (NumberFormatException e) {
			}
		}
		return constantDetails;
	}
	
	public int getCode(String name) {
		if (name == null) { return Constants.NULL_INTEGER;}
		ConstantDetails constantDetails = nameLookup.get(name.toLowerCase());
		if (constantDetails != null) {
			return constantDetails.getCode();
		}
		
		int code = Constants.NULL_INTEGER;
		
		try {
			code = Integer.parseInt(name);
		} catch (NumberFormatException e) {		}
		return code;
	}


	@Override
	public String getClassSimpleName() {
		return simpleName;
	}

	@Override
	public String getClassFullName() {
		return fullName;
	}

}
