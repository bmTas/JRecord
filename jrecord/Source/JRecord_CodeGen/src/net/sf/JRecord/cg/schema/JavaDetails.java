package net.sf.JRecord.cg.schema;

import net.sf.JRecord.cg.common.CCode;

public class JavaDetails {
	private final String cobolName, extensionName, javaName, className;

	protected JavaDetails(String cobolName) {
		super();
		
		StringBuilder b = CCode.cobolName2JavaName(cobolName);
		this.cobolName = cobolName;
	
		this.extensionName = CCode.toSuffix(b);
		this.javaName = CCode.toFieldName(b);
		this.className = CCode.toClassName(b);
	}

	/**
	 * @return the cobolName
	 */
	public final String getCobolName() {
		return cobolName;
	}

	/**
	 * @return the extensionName
	 */
	public final String getExtensionName() {
		return extensionName;
	}

	/**
	 * @return the javaName
	 */
	public final String getJavaName() {
		return javaName;
	}

	/**
	 * @return the className
	 */
	public final String getClassName() {
		return className;
	}
	
	
}
