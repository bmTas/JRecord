package net.sf.JRecord.cg;

import net.sf.JRecord.cg.details.GenerateOptions;
import net.sf.JRecord.cg.details.ParseArgs;
import net.sf.JRecord.cg.velocity.GenerateVelocity;

public class Generate {

	public static void main(String[] args) {
		ParseArgs pa = new ParseArgs(args);
		
		if (pa.get2Args("-h", "-help", pa.getArg("-?")) != null) {
			GenerateOptions.printOptions();
		} else {
			GenerateOptions opts = new GenerateOptions(pa);
			
			if (opts.isOk()) {
				new GenerateVelocity( opts);
			}
		}

	}

}
