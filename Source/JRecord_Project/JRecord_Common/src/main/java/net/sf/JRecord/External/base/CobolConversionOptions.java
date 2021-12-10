package net.sf.JRecord.External.base;

import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;

/**
 * This class holds basic options to convert Cobol-Copybooks
 * to JRecord LayoutDetails
 * 
 * @author Bruce Martin
 *
 */
public class CobolConversionOptions {
	
	public static final CobolConversionOptions STANDARD_OPTIONS 
			= new CobolConversionOptions(ICopybookDialects.FMT_MAINFRAME, ICobolSplitOptions.SPLIT_NONE, false);
	
	
	public final int cobolDialect, splitCopybookOption;
	public final boolean dropCopybookName;
	
	public CobolConversionOptions(int cobolDialect, int splitCopybookOption, boolean dropCopybookName) {
		super();
		this.cobolDialect = cobolDialect;
		this.splitCopybookOption = splitCopybookOption;
		this.dropCopybookName = dropCopybookName;
	}
}
