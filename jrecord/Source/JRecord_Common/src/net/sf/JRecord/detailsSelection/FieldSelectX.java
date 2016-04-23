/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.detailsSelection;

import java.math.BigDecimal;
import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;

public abstract class FieldSelectX extends FieldSelect {
	public static final String STARTS_WITH  = Constants.STARTS_WITH;
	public static final String DOES_NOT_CONTAIN  = Constants.DOES_NOT_CONTAIN;
	public static final String CONTAINS= Constants.CONTAINS;
	public static final String EMPTY   = Constants.EMPTY;
	public static final String NUM_EQ  = Constants.NUM_EQ;
	public static final String NUM_GT  = Constants.NUM_GT;
	public static final String NUM_GE  = Constants.NUM_GE;
	public static final String NUM_LT  = Constants.NUM_LT;
	public static final String NUM_LE  = Constants.NUM_LE;
	public static final String TEXT_EQ = Constants.TEXT_EQ;
	public static final String TEXT_GT = Constants.TEXT_GT;
	public static final String TEXT_GE = Constants.TEXT_GE;
	public static final String TEXT_LT = Constants.TEXT_LT;
	public static final String TEXT_LE = Constants.TEXT_LE;

	public static final int G_FIRST  = GetValue.GT_FIRST;
	public static final int G_LAST   = GetValue.GT_LAST;
	public static final int G_MAX    = GetValue.GT_MAX;
	public static final int G_MIN    = GetValue.GT_MIN;
	public static final int G_SUM    = GetValue.GT_SUM;
	public static final int G_AVE    = GetValue.GT_AVE;
	public static final int G_ANY_OF = GetValue.GT_MAXIMUM_ID + 1;
	public static final int G_ALL    = GetValue.GT_MAXIMUM_ID + 2;
	public static final int G_HIGHEST_CODE = GetValue.GT_MAXIMUM_ID + 2;

	private boolean numeric;
	protected BigDecimal num;

	public FieldSelectX(String name, String value, String op, IGetValue fieldDef) {
		this(	name, value, op,
				fieldDef.isNumeric(),
				fieldDef);
	}

	public FieldSelectX(String name, String value, String op, boolean isNumeric, IGetValue fieldDef) {
		super(name, value, op, fieldDef);
		BigDecimal d = null;

		if (getValue == null) {
			numeric = false;
		} else {
			numeric = isNumeric;

			if (numeric) {
				d = new BigDecimal(value);
			}
		}
		num = d;
	}
	public static FieldSelect get(ExternalFieldSelection fs, IFieldDetail fieldDef) {
		return get(fs.getFieldName(), fs.getFieldValue(), fs.getOperator(), -1, fieldDef, fs.isCaseSensitive());
	} 

//	public static FieldSelect get(String name, String value, String op, FieldDetail fieldDef) {
//		FieldSelect ret;
//		if ("!=".equals(op) || "ne".equalsIgnoreCase(op)) {
//			ret = new FieldSelect.NotEqualsSelect(name, value, fieldDef);
//		} else if (">".equals(op) || "gt".equalsIgnoreCase(op)) {
//			ret = new FieldSelectX.GreaterThan(name, value, ">", fieldDef);
//		} else if (">=".equals(op) || "ge".equalsIgnoreCase(op)) {
//			ret = new FieldSelectX.GreaterThan(name, value,  ">=", fieldDef);
//		} else if ("<".equals(op) || "lt".equalsIgnoreCase(op)) {
//			ret = new FieldSelectX.LessThan(name, value, "<", fieldDef);
//		} else if ("<=".equals(op) || "le".equalsIgnoreCase(op)) {
//			ret = new FieldSelectX.LessThan(name, value, "<=", fieldDef);
//		} else {
//			ret = new FieldSelect.EqualsSelect(name, value, fieldDef);
//		}
//
//		return ret;
//	}

	public static RecordSel get(String name, String value, String op, int groupId, int recordIdx, IFieldDetail fieldDef, boolean caseSensitive) {
		switch (groupId) {
		case G_ALL:
		case G_ANY_OF:
			FieldSelect r = get(name, value, op, recordIdx, fieldDef, caseSensitive);
			return new AnyAllOf(r, groupId == G_ANY_OF);
		default:
			GetValue g = GetValue.get(groupId, fieldDef, recordIdx);
			if (g != null) {
				FieldSelect rs = get(name, value, op, g, caseSensitive);
				rs.setCaseSensitive(caseSensitive);
				return rs;
			}
		}
		throw new RuntimeException("No valid grouping function !!!");
	}

	public static FieldSelect get(String name, String value, String op, IFieldDetail fieldDef) {
		return get(name, value, op, new GetValue.FieldValue(fieldDef, -1), false);
	}

	public static FieldSelect get(String name, String value, String op, int recordIdx, IFieldDetail fieldDef, boolean caseSensitive) {
		return get(name, value, op, new GetValue.FieldValue(fieldDef, recordIdx), caseSensitive);
	}

	public static FieldSelect get(String name, String value, String op, IGetValue fieldDef, boolean caseSensitive) {
		FieldSelect ret ;
		if (op != null) {
			op = op.trim();
		}
		if ("!=".equals(op) || "<>".equals(op) ||"ne".equalsIgnoreCase(op)) {
			ret = new FieldSelectX.NotEqualsSelect(name, value, fieldDef);
		} else if (CONTAINS.equalsIgnoreCase(op)) {
			ret = new FieldSelect.Contains(name, value, fieldDef);
		} else if (EMPTY.equalsIgnoreCase(op)) {
			ret = new FieldSelect.Empty(name, value, fieldDef);
		} else if (DOES_NOT_CONTAIN.equalsIgnoreCase(op)) {
			ret = new FieldSelect.DoesntContain(name, value, fieldDef);
		} else if (STARTS_WITH.equalsIgnoreCase(op)) {
			ret = new FieldSelect.StartsWith(name, value, fieldDef);
		} else {
			ret = getBasic(name, value, op, fieldDef, caseSensitive);
		}

		if (ret == null) {
			if (	   NUM_EQ.equalsIgnoreCase(op)	 || Constants.NUM_NE.equalsIgnoreCase(op)
					|| NUM_GT.equalsIgnoreCase(op)   || NUM_GE.equalsIgnoreCase(op)
					|| NUM_LT.equalsIgnoreCase(op)   || NUM_LE.equalsIgnoreCase(op)) {
				FieldSelectX ret1 = getBasic(name, value, op.substring(0, 2).trim(), fieldDef, caseSensitive);
				ret1.setNumeric(true);
				ret = ret1;
			} else if (TEXT_EQ.equalsIgnoreCase(op)	 || Constants.TEXT_NE.equalsIgnoreCase(op)
					|| TEXT_GT.equalsIgnoreCase(op)	 || TEXT_GE.equalsIgnoreCase(op)
					|| TEXT_LT.equalsIgnoreCase(op)  || TEXT_LE.equalsIgnoreCase(op)) {
				FieldSelectX ret1 = getBasic(name, value, op.substring(0, 2).trim(), fieldDef, caseSensitive);
				ret1.setNumeric(false);
				ret = ret1;
			} else {
				ret = new FieldSelectX.EqualsSelect(name, value, fieldDef);
			}
		}

		return ret;
	}

	public static FieldSelect getTrueSelection() {
		return new FieldSelect.TrueSelect();
	}

//	private static FieldSelectX getBasic(String name, String value, String op, int recordIdx, FieldDetail fieldDef) {
//		return getBasic(name, value, op, new GetValue.FieldGetValue(fieldDef, recordIdx));
//	}

	private static FieldSelectX getBasic(String name, String value, String op, IGetValue fieldDef, boolean caseSensitive) {
		FieldSelectX ret = null;
		if ("=".equals(op)) {
			ret = new FieldSelectX.EqualsSelect(name, value, fieldDef);
		} else if (">".equals(op) || "gt".equalsIgnoreCase(op)) {
			ret = new FieldSelectX.GreaterThan(name, value, ">", fieldDef);
		} else if (">=".equals(op) || "ge".equalsIgnoreCase(op)) {
			ret = new FieldSelectX.GreaterThan(name, value,  ">=", fieldDef);
		} else if ("<".equals(op) || "lt".equalsIgnoreCase(op)) {
			ret = new FieldSelectX.LessThan(name, value, "<", fieldDef);
		} else if ("<=".equals(op) || "le".equalsIgnoreCase(op)) {
			ret = new FieldSelectX.LessThan(name, value, "<=", fieldDef);
		} else if ("<>".equals(op)) {
			ret = new FieldSelectX.NotEqualsSelect(name, value, fieldDef);
		}

		if (ret != null) {
			ret.setCaseSensitive(caseSensitive);
		}
		return ret;
	}

	protected final int compare(Object o, int defaultVal) {
		int res = defaultVal;
		if (o == null || o.toString() == null) return Constants.NULL_INTEGER;
		if (numeric) {
			if (o instanceof BigDecimal) {
				res = ((BigDecimal) o).compareTo(num);
			} else {
				try {
					res = new BigDecimal(o.toString()).compareTo(num);
					//System.out.println(" -->> " + res + " " + o.toString() + " " + num);
				} catch (Exception e) {
				}
			}
		} else {
			String fieldValue = getFieldValue();
			if (fieldValue == null) {

			} else if (isCaseSensitive()){
				res = o.toString().compareTo(fieldValue);
			} else {
				res = o.toString().compareToIgnoreCase(fieldValue);
			}
		}
		return res;
	}


	/**
	 * @return the numeric
	 */
	public boolean isNumeric() {
		return numeric;
	}

	/**
	 * @param numeric the numeric to set
	 */
	public void setNumeric(boolean numeric) {

		if (numeric && num == null) {
			try {
				num = new BigDecimal(super.getFieldValue());
				this.numeric = numeric;
			} catch (Exception e) {
				// TODO: handle exception
			}
		} else {
			this.numeric = numeric;
		}
	}


	public static final class GreaterThan extends FieldSelectX {
		private int cmpTo = 0;
		private GreaterThan(String name, String value, String op, IGetValue fieldDef) {
			super(name, value, op, fieldDef);

			if (">".equals(op)) {
				cmpTo = 1;
			}
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object value) {
			 return compare(value, -2) >= cmpTo;
		}
	}

	public static final class LessThan extends FieldSelectX {
		private int cmpTo = 0;
		private LessThan(String name, String value, String op, IGetValue fieldDef) {
			super(name, value, op, fieldDef);

			if ("<".equals(op)) {
				cmpTo = -1;
			}
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object value) {
			int cmp = compare(value, 2);
			return  cmp <= cmpTo && cmp != Constants.NULL_INTEGER;
		}
	}


	public static class EqualsSelect extends FieldSelectX {

		protected EqualsSelect(String name, String value, IGetValue fieldDef) {
			super(name, value, "=", fieldDef);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object value) {
			return compare(value, 2) == 0;
		}
	}

	public static class NotEqualsSelect extends FieldSelectX {

		protected NotEqualsSelect(String name, String value, IGetValue fieldDef) {
			super(name, value, "!=", fieldDef);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object value) {

			return compare(value, 0) != 0;		}
	}

	public static abstract class DelagteRecordSel implements RecordSel {
		private final RecordSel childSel;



		public DelagteRecordSel(RecordSel childSel) {
			super();
			this.childSel = childSel;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getType()
		 */
		@Override
		public int getType() {
			return childSel.getType();
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getElementCount()
		 */
		@Override
		public int getElementCount() {
			return childSel.getElementCount();
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.RecordSel#isSelected(net.sf.JRecord.Common.AbstractIndexedLine)
		 */
		@Override
		public boolean isSelected(AbstractIndexedLine line) {
			return childSel.isSelected(line);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.RecordSel#isIncluded(net.sf.JRecord.Common.AbstractIndexedLine)
		 */
		@Override
		public boolean isIncluded(AbstractIndexedLine line) {
			return childSel.isIncluded(line);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.RecordSel#getFirstField()
		 */
		@Override
		public FieldSelect getFirstField() {
			return childSel.getFirstField();
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.RecordSel#getAllFields(java.util.List)
		 */
		@Override
		public void getAllFields(List<FieldSelect> fields) {
			childSel.getAllFields(fields);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.RecordSel#getSize()
		 */
		@Override
		public int getSize() {
			return childSel.getSize();
		}
	}

	public static class AnyAllOf extends DelagteRecordSel {
		private final boolean anyOf;
		public AnyAllOf(RecordSel childSel, boolean any) {
			super(childSel);

			anyOf = any;
		}

		/**
		 * @see net.sf.JRecord.detailsSelection.RecordSel#isSelected(java.util.List)
		 */
		@Override
		public boolean isSelected(List<? extends AbstractIndexedLine> lines) {
			if (lines == null) return false;
			for (AbstractIndexedLine l : lines) {
				if (isIncluded(l) && (isSelected(l) == anyOf)) {
					return anyOf;
				}
			}

			return ! anyOf;
		}
	}

}

