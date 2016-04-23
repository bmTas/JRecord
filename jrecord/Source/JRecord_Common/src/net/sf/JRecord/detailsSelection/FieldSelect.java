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

import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.IEmptyTest;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;

public abstract class FieldSelect extends ExternalFieldSelection implements RecordSel {

	//protected final FieldDetail fieldDetail;
	protected IGetValue getValue;


	public FieldSelect(String name, String value, String op, IGetValue getValue) {
		super(name, value, op);
		this.getValue = getValue;
		if (getValue == null) {
			this.getValue = new IGetValue() {

				@Override
				public boolean isNumeric() {
					return false;
				}

				@Override
				public Object getValue(List<? extends AbstractIndexedLine> lines) {
					return null;
				}

				@Override
				public Object getValue(AbstractIndexedLine line) {
					return null;
				}

				/* (non-Javadoc)
				 * @see net.sf.JRecord.detailsSelection.IGetValue#isIncluded(net.sf.JRecord.Common.AbstractIndexedLine)
				 */
				@Override
				public boolean isIncluded(AbstractIndexedLine line) {
					return true;
				}
			};
		}
	}





	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSel#getAllFields(java.util.List)
	 */
	@Override
	public void getAllFields(List<FieldSelect> fields) {
		fields.add(this);
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSel#getFirstField()
	 */
	@Override
	public FieldSelect getFirstField() {
		return this;
	}





	/* (non-Javadoc)
	 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getElementCount()
	 */
	@Override
	public int getElementCount() {
		return 1;
	}



	public final boolean isSelected(List<? extends AbstractIndexedLine> lines) {
		return isSelected(getValue.getValue(lines));
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.detailsSelection.RecordSel#isIncluded(net.sf.JRecord.Common.AbstractIndexedLine)
	 */
	@Override
	public boolean isIncluded(AbstractIndexedLine line) {
		//if (isIncluded(line)) {
		return getValue.isIncluded(line);
		//}
		//return false;
	}





	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSel#isSelected(net.sf.JRecord.Details.AbstractLine)
	 */
	@Override
	public final boolean isSelected(AbstractIndexedLine line) {
		return isSelected(getValue.getValue(line));
	}

	public abstract boolean isSelected(Object value);

	public static class Contains extends FieldSelect {

		protected Contains(String name, String value, IGetValue getValue) {
			super(name, value, "Contains", getValue);
		}


		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object o) {

			if (super.isCaseSensitive() || o == null) {
				return  o != null
					&& (o.toString().indexOf(getFieldValue()) >= 0);
			}
			return (o.toString().toLowerCase().indexOf(getFieldValue()) >= 0);
		}
	}

	public static class DoesntContain extends FieldSelect {

		protected DoesntContain(String name, String value, IGetValue fieldDef) {
			super(name, value, "Doesnt_Contain", fieldDef);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object o ) {

			if (super.isCaseSensitive() || o == null) {
				return  o == null
					|| (o.toString().indexOf(getFieldValue()) < 0);
			}
			return (o.toString().toLowerCase().indexOf(getFieldValue()) < 0);
		}
	}

	public static class StartsWith extends FieldSelect {

		protected StartsWith(String name, String value, IGetValue fieldDef) {
			super(name, value, "Doesnt_Contain", fieldDef);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object o) {

			if (super.isCaseSensitive() || o == null) {
				return  o != null
					&& (o.toString().startsWith(getFieldValue()));
			}
			return (o.toString().toLowerCase().startsWith(getFieldValue()));
		}
	}

	public static class Empty extends FieldSelect {

		protected Empty(String name, String value, IGetValue fieldDef) {
			super(name, value, "Doesnt_Contain", fieldDef);
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object o) {
			return o == null
				|| o.toString() == null
				|| o.toString().trim().length() == 0
				|| (o instanceof IEmptyTest && ((IEmptyTest) o).isEmpty());
		}
	}

	public static class TrueSelect extends FieldSelect {

		protected TrueSelect() {
			super("", "", "True", null);

		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.Details.Selection.RecordSelection#isSelected(net.sf.JRecord.Details.AbstractLine)
		 */
		@Override
		public boolean isSelected(Object o) {
			return true;
		}


	}

	/**
	 * @return the fieldDetail
	 */
	public IFieldDetail getFieldDetail() {
		if (getValue instanceof GetValue) {
			return ((GetValue) getValue).fieldDetail;
		}
		return null;
	}
}
