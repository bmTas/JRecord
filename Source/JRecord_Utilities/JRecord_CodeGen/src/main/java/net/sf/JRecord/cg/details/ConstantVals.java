package net.sf.JRecord.cg.details;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public class ConstantVals {

	public static final ConstantVals CONSTANT_VALUES = new ConstantVals();
	
	public final int TYPE_ATOM =  ExternalSelection.TYPE_ATOM;
	public final int TYPE_AND  =  ExternalSelection.TYPE_AND;
	public final int TYPE_OR   =  ExternalSelection.TYPE_OR;

	/**
	 * @return the TYPE_ATOM
	 */
	public final int getTYPE_ATOM() {
		return TYPE_ATOM;
	}

	/**
	 * @return the TYPE_AND
	 */
	public final int getTYPE_AND() {
		return TYPE_AND;
	}

	/**
	 * @return the TYPE_OR
	 */
	public final int getTYPE_OR() {
		return TYPE_OR;
	}
}
