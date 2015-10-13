package net.sf.JRecord.Option;

public class OptionType implements IRecordPositionOption {
	
	public static final OptionType REQUIRED = new OptionType("Required");
	
	public final String name;
	public OptionType(String name) {
		this.name = name;
	}
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Option.IOptionType#getName()
	 */
	@Override
	public String getName() {
		return name;
	}
	
	
}
