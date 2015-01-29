package net.sf.JRecord.Option;

public class OptionType implements IOptionType {
	
	public static final OptionType REQUIRED = new OptionType("Required");
	
	public final String name;
	public OptionType(String name) {
		this.name = name;
	}
}
