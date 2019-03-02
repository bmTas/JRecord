package net.sf.JRecord.cg.schema;

import net.sf.JRecord.Common.Conversion;

/**
 * This class provides the File name in a variety of formats
 * 
 * @author Bruce Martin
 *
 */
public final class CodeGenFileName {
	public final String fileName;

	public CodeGenFileName(String fileName) {
		super();
		this.fileName = fileName;
	}

	/**
	 * Get the file-name (C:\MyDir\...)
	 * @return the fileName
	 */
	public final String getFileName() {
		return fileName;
	}


	/**
	 * Get the file name in Java format (C:/MyDir/...)
	 * 
	 * @return the fileName
	 */
	public final String getJavaFileName() {
		return Conversion.replace(fileName, "\\", "/").toString();
	}


	/**
	 * Get the file name in escaped format 
	 * so it can be used in a Java String (C:\\MyDir\\...)
	 * 
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
