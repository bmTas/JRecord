package net.sf.JRecord.External;

public class Cb2xmlDocument {

	public final int splitOption;
	public final String splitAtLevel;
	public final Object cb2xmlDocument;
	
	protected Cb2xmlDocument(int splitOption, String splitAtLevel, Object cb2xmlDocument) {
		super();
		this.splitOption = splitOption;
		this.splitAtLevel = splitAtLevel;
		this.cb2xmlDocument = cb2xmlDocument;
	}

}
