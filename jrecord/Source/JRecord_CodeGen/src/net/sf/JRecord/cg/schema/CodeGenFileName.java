package net.sf.JRecord.cg.schema;

import net.sf.JRecord.Common.Conversion;

public final class CodeGenFileName {
	public final String fileName;

	public CodeGenFileName(String fileName) {
		super();
		this.fileName = fileName;
	}

	/**
	 * @return the fileName
	 */
	public final String getFileName() {
		return fileName;
	}


	/**
	 * @return the fileName
	 */
	public final String getJavaFileName() {
		return Conversion.replace(fileName, "\\", "/").toString();
	}


	/**
	 * @return the fileName
	 */
	public final String getEscapedFileName() {
		return Conversion.replace(fileName, "\\", "\\\\").toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return fileName;
	}

}
