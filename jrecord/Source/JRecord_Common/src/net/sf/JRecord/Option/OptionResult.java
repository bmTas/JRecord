package net.sf.JRecord.Option;

public class OptionResult implements IOptionResult {

	public static OptionResult NO  = new OptionResult("no");
	public static OptionResult YES = new OptionResult("yes");
	public static OptionResult UNKOWN = new OptionResult("unknown");
	
	public final String name;
	public OptionResult(String name) {
		this.name = name;
	}

	
	public static OptionResult get(boolean b) {
		return b?YES:NO;
	}
}
