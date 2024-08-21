package net.sf.JRecord.ExternalRecordSelection;

import net.sf.JRecord.Common.ISelectionOperators;

/**
 * Create Record-Selection
 * @author Bruce Martin
 *
 */
public class ExternalSelectionBuilder {
	
	/**
	 * Create <i>Field Selection</i> using the equals operator
	 * @param fieldName field name to test
	 * @param value value to test the field against against
	 * @return wether the field equals the supplied value
	 */
	public ExternalFieldSelection newFieldSelectionEquals(String fieldName, String value) {
		return new ExternalFieldSelection(fieldName, value);
	}
	
	/**
	 *  Create <i>Field Selection</i> using a supplied operator operator
	 * @param fieldName  field name to test
	 * @param operator operator used in the test. Options include =, >, <, >=, !=. See {@link ISelectionOperators} 
	 * for the list.
	 * @param value value to test the field against against
	 * @return wether passed the Test
	 */
	public ExternalFieldSelection newFieldSelection(String fieldName, String operator, String value) {
		return new ExternalFieldSelection(fieldName, value, operator);
	}

	/**
	 * Perform a boolean `And` between one or more External-Selections
	 * @param selections Field/And/Or Selection to perform a boolean AND on 
	 * @return boolean result of boolean <b>and</b>
	 */
	public ExternalGroupSelection<ExternalSelection> newAnd(ExternalSelection... selections) {
		return ExternalGroupSelection.newAnd(selections);
	}
	
	/**
	 * Perform a boolean `OR` between one or more External-Selections
	 * @param selections Field/And/Or Selection to perform a boolean AND on 
	 * @return boolean result of <b>or</b>
	 */
	public ExternalGroupSelection<ExternalSelection> newOr(ExternalSelection... selections) {
		return ExternalGroupSelection.newOr(selections);
	}

}
