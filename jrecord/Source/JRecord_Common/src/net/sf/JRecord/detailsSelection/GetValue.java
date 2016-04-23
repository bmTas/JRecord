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
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;

/**
 * Accumulators These
 * @author mum
 *
 */
public abstract class GetValue  implements IGetValue {

	public static final int GT_FIELD = 0;
	public static final int GT_FIRST = 1;
	public static final int GT_LAST  = 2;
	public static final int GT_MAX   = 3;
	public static final int GT_MIN   = 4;
	public static final int GT_SUM   = 5;
	public static final int GT_AVE   = 6;
	public static final int GT_MAXIMUM_ID = 6;

	public static GetValue get(int type, IFieldDetail fieldDetail, int recordIdx) {

		switch (type) {
		case GT_FIELD: return new FieldValue(fieldDetail, recordIdx);
		case GT_FIRST: return new First(fieldDetail, recordIdx);
		case GT_LAST : return new Last(fieldDetail, recordIdx);
		case GT_MIN  : return new Min(fieldDetail, recordIdx);
		case GT_MAX  : return new Max(fieldDetail, recordIdx);
		case GT_SUM  : return new Sum(fieldDetail, recordIdx, false);
		case GT_AVE  : return new Sum(fieldDetail, recordIdx, true);
		}
		throw new RuntimeException("Invalid Accumulator: " + type);
	}

	private static BigDecimal getNum(Object o) {
		if (o == null) {
			return null;
		} else if (o instanceof BigDecimal) {
			return (BigDecimal) o;
		} else  {
			try {
				return new BigDecimal(o.toString());
			} catch (Exception e) {
			}
		}
		return null;
	}

	private static String getString(Object o) {
		if (o == null) {
			return null;
		} else  {
			return o.toString();
		}
	}


//	private abstract static class Cmp implements IGetValue {
	protected final IFieldDetail fieldDetail;
	private final int recordIdx;



	public GetValue(IFieldDetail fieldDetail, int recordIdx) {
		super();
		this.fieldDetail = fieldDetail;
		this.recordIdx = recordIdx;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#isNumeric()
	 */
	@Override
	public boolean isNumeric() {
		return fieldDetail != null && TypeManager.getInstance().getType(fieldDetail.getType()).isNumeric();
	}


	/**
	 * @see net.sf.JRecord.detailsSelection.IGetValue#getValue(net.sf.JRecord.Common.AbstractIndexedLine)
	 */
	@Override
	public Object getValue(AbstractIndexedLine line) {
		if (fieldDetail != null && (recordIdx < 0 || recordIdx == line.getPreferredLayoutIdx())) {
			//System.out.print(" >> " + line.getField(fieldDetail) + " " + fieldDetail.getName());
			return line.getField(fieldDetail);
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsSelection.IGetValue#isIncluded(net.sf.JRecord.Common.AbstractIndexedLine)
	 */
	@Override
	public boolean isIncluded(AbstractIndexedLine line) {
		return (recordIdx < 0 || recordIdx == line.getPreferredLayoutIdx());
	}

	/**
	 * @return the fieldDetail
	 */
	public IFieldDetail getFieldDetail() {
		return fieldDetail;
	}

//	}

	public static class FieldValue extends GetValue {

		public FieldValue(IFieldDetail fieldDetail, int recordIdx) {
			super( fieldDetail, recordIdx);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			throw new RuntimeException("passed a list of lines");
		}
	}

	public static class Max extends GetValue {

		public Max(IFieldDetail fieldDetail, int recordIdx) {
			super(fieldDetail, recordIdx);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			if (TypeManager.getInstance().getType(fieldDetail.getType()).isNumeric()) {
				BigDecimal max = null,
						   c;
				for (AbstractIndexedLine l : lines) {
					c = getNum(getValue(l));
					if (c != null && (max == null || c.compareTo(max) > 0)) {
						max = c;
					}
				}
				return max;
			} else {
				String s;
				String maxStr = null;
				for (AbstractIndexedLine l : lines) {
					s = getString(getValue(l));
					if (s != null && (maxStr == null || s.compareTo(maxStr) > 0)) {
						maxStr = s;
					}
				}
				return maxStr;
			}
		}
	}

	public static class Min extends GetValue {

		public Min(IFieldDetail fieldDetail, int recordIdx) {
			super(fieldDetail, recordIdx);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			if (TypeManager.getInstance().getType(fieldDetail.getType()).isNumeric()) {
				BigDecimal max = null,
						   c;
				for (AbstractIndexedLine l : lines) {
					c = getNum(getValue(l));
					if (c != null && (max == null || c.compareTo(max) < 0)) {
						max = c;
					}
				}
				return max;
			} else {
				String s;
				String maxStr = null;
				for (AbstractIndexedLine l : lines) {
					s = getString(getValue(l));
					if (s != null && (maxStr == null || s.compareTo(maxStr) < 0)) {
						maxStr = s;
					}
				}
				return maxStr;
			}
		}
	}

	public static class First extends GetValue {

		public First(IFieldDetail fieldDetail, int recordIdx) {
			super(fieldDetail, recordIdx);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			Object o;
			for (int i = 0; i < lines.size(); i++) {
				o = getValue(lines.get(i));
				if (o != null) {
					return o;
				}
			}
			return null;

		}
	}


	public static class Last extends GetValue {

		public Last(IFieldDetail fieldDetail, int recordIdx) {
			super(fieldDetail, recordIdx);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			Object o;
			for (int i = lines.size()-1; i >= 0; i--) {
				o = getValue(lines.get(i));
				if (o != null) {
					return o;
				}
			}
			return null;
		}
	}

	public static class Sum extends GetValue {

		boolean calcAverage;
		public Sum(IFieldDetail fieldDetail, int recordIdx, boolean ave) {
			super(fieldDetail, recordIdx);

			calcAverage = ave;
		}


		/* (non-Javadoc)
		 * @see net.sf.JRecord.detailsSelection.copy.IGetValue#getValue(java.util.List)
		 */
		@Override
		public Object getValue(List<? extends AbstractIndexedLine> lines) {
			BigDecimal sum = BigDecimal.ZERO,
						   c;
			if (TypeManager.getInstance().getType(fieldDetail.getType()).isNumeric()) {
				int count = 0;
				for (AbstractIndexedLine l : lines) {
					c = getNum(getValue(l));
					if (c != null ) {
						sum = sum.add(c);
						count +=1;
					}
				}
				if (count > 0 && calcAverage) {
					sum = sum.divide(BigDecimal.valueOf(count), 10, BigDecimal.ROUND_UP);
				}
			}
			return sum;
		}
	}

}
