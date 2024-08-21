package net.sf.JRecord.Common;

public interface ISelectionOperators {

	public static final String STARTS_WITH  = "Starts With";
	public static final String DOES_NOT_CONTAIN  = "Doesn't Contain";
	public static final String CONTAINS  = "Contains";
	public static final String EMPTY   = "Is Empty";
	public static final String HAS_DATA   = "Has Data";
	public static final String NUM_EQ  = "= (Numeric)";
	public static final String NUM_GT  = "> (Numeric)";
	public static final String NUM_GE  = ">= (Numeric)";
	public static final String NUM_LT  = "< (Numeric)";
	public static final String NUM_LE  = "<= (Numeric)";
	public static final String TEXT_EQ = "= (Text)";
	public static final String TEXT_GT = "> (Text)";
	public static final String TEXT_GE = ">= (Text)";
	public static final String TEXT_LT = "< (Text)";
	public static final String TEXT_LE = "<= (Text)";
	public static final String NUM_NE  = "<> (Numeric)";
	public static final String TEXT_NE = "<> (Text)";
	public static final String REG_EXP = "Regular Expression";
	public static final String ANY_VALUE = "Any Value";

	public static final String[] VALID_COMPARISON_OPERATORS = {
		"=", "eq", "!=", "<>", "ne", ">", "gt", ">=", "ge", "<", "lt", "<=", "le",
		STARTS_WITH,  DOES_NOT_CONTAIN,  CONTAINS,
		NUM_EQ,   NUM_GT,   NUM_GE,   NUM_LT,   NUM_LE,
		TEXT_EQ,  TEXT_GT,  TEXT_GE,  TEXT_LT,  TEXT_LE,
		NUM_NE,   TEXT_NE, EMPTY, HAS_DATA, REG_EXP, ANY_VALUE
	};

}
