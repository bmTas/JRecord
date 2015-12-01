package net.sf.JRecord.ExternalRecordSelection;

/**
 * Purpose Streamline (remove unnecessary groups) from a Selection
 * equation. Basically it performs very basic optimization on the equation
 * 
 * Each <i>ExternalSelection</i> represents an boolean-equation and it can
 * be one of
 * <ul>
 *   <li><b>Or-group</b> which has one or more child <i>ExternalSelection</i> 
 *   <li><b>And-group</b> which has one or more child <i>ExternalSelection</i> 
 *   <li><b>Field-Test</b>Test a field against a value
 * </ul
 *    
 * @author Bruce Martin
 *
 * @param <Selection> Equation type
 */
public class StreamLine<Selection extends ExternalSelection> {
	
	private static final StreamLine<ExternalSelection> externalStreamLine
				= new StreamLine<ExternalSelection>();
	
	/**
	 * Streamline (optimize - remove unnessary groups) the equation
	 * @param sel boolean equation 
	 * @return optimized equation 
	 */
	public final Selection streamLine(Selection sel) {
		if (sel instanceof ExternalGroupSelection) {
			ExternalGroupSelection<Selection> grp = (ExternalGroupSelection<Selection>) sel;
			if (grp.size() == 1) {
				return streamLine(grp.get(0));
			} else {
				for (int i = 0; i < grp.size(); i++) {
					
					grp.set(i, streamLine(grp.get(i)));
				}
			}
		}
		return sel;
	}

	/**
	 * @return the externalStreamLine
	 */
	public static StreamLine<ExternalSelection> getExternalStreamLine() {
		return externalStreamLine;
	}
}
