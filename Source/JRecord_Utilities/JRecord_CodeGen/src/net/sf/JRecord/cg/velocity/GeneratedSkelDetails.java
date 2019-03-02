package net.sf.JRecord.cg.velocity;

public class GeneratedSkelDetails {
	public final String filename, javaFilename, description;
	private String generatedCode;
	
	GeneratedSkelDetails(String filename, String javaFilename,
			String description) {
		super();
		this.filename = filename;
		this.javaFilename = javaFilename;
		this.description = description;
	}

	/**
	 * @return the generatedCode
	 */
	public final String getGeneratedCode() {
		return generatedCode;
	}

	/**
	 * @param generatedCode the generatedCode to set
	 */
	public final void setGeneratedCode(String generatedCode) {
		this.generatedCode = generatedCode;
	}
	
}
