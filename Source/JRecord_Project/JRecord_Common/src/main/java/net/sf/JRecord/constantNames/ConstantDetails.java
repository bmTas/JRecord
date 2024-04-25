package net.sf.JRecord.constantNames;

/**
 * JRecord contains many integer codes. This class holds a Code and String equivalent values
 * 
 * @author Bruce Martin
 *
 */
public class ConstantDetails {
	private static final IInterfaceDetails EMPTY_INTERFACE_DETAILS = new IInterfaceDetails() {		
		@Override public String getClassSimpleName() { return ""; }
		
		@Override public String getClassFullName() { return ""; }
	};
	private IInterfaceDetails parentInterfaceDetails = EMPTY_INTERFACE_DETAILS;
	private final int code;
	private final String simpleName, externalName, jrecordConstant;
	private String[] extraNames;
	
	public ConstantDetails(int code, String simpleName, String externalName, String jrecordConstant) {
		super();
		this.code = code;
		this.simpleName = simpleName;
		this.externalName = externalName;
		this.jrecordConstant = jrecordConstant;
	}
	
	ConstantDetails setExtraNames(String... names) {
		extraNames = names;
		return this;
	}

	/**
	 * @return the extraNames
	 */
	String[] getExtraNames() {
		return extraNames;
	}

	/**
	 * @return the code
	 */
	public int getCode() {
		return code;
	}

	/**
	 * @return the simpleName
	 */
	public String getSimpleName() {
		return simpleName;
	}

	/**
	 * @return the externalName
	 */
	public String getExternalName() {
		return externalName;
	}

	/**
	 * @return the jrecordConstant
	 */
	String getJRecordConstant() {
		return jrecordConstant;
	}
	
	/**
	 * @return the jrecordConstant
	 */
	public String getJRecordInterfaceConstant() {
		return parentInterfaceDetails.getClassSimpleName() + '.' + jrecordConstant;
	}

	/**
	 * @param parentInterfaceDetails the parentInterfaceDetails to set
	 */
	public void setParentInterfaceDetails(IInterfaceDetails parentInterfaceDetails) {
		this.parentInterfaceDetails = parentInterfaceDetails;
	}
	
	
	
}
