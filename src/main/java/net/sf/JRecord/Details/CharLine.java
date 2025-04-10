/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.Details;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.BaseType;
import net.sf.JRecord.Types.TypeManager;

public class CharLine extends BasicLine implements Cloneable {

	private static final LineProvider defaultProvider = new CharLineProvider();

	private String data;

	public CharLine(LayoutDetail layoutDef, String line) {
		super(defaultProvider, layoutDef);

		if (line == null) {
			data = "";
		} else {
			data = line;
		}
	}


	@Override
	public byte[] getData(int start, int len) {
		String tmpData = data;
	    int tempLen = Math.min(len, tmpData.length() - start + 1);

	    if ((tmpData.length() < start) || (tempLen < 1) || (start < 1)) {
	        return NULL_RECORD;
	    }

		return Conversion.getBytes(
				tmpData.substring(
						start - 1,
						start + tempLen - 1
				),
				layout.getFontName());
	}


	@Override
	public byte[] getData() {
		return Conversion.getBytes(data, layout.getFontName());
	}



	@SuppressWarnings("deprecation")
	@Override
	public Object getField(int typeId, IFieldDetail field) {

		if (field.isFixedFormat()) {
			if (typeId == Type.ftChar
			||  typeId == Type.ftCharRightJust
			||  typeId == Type.ftCharRestOfRecord 
			||  TypeManager.getInstance().getType(typeId) == TypeManager.getInstance().getType(Type.ftChar)) {
				return getFieldText(field);
			}

			Type type = TypeManager.getSystemTypeManager().getType(typeId);

            int pos =  getFieldsPosition(field);
            
			byte[] bytes = getData(pos, field.getLen());
			FieldDetail tmpField = new FieldDetail(field.getFontName(), field.getDescription(),
					typeId, field.getDecimal() ,field.getFontName(), field.getFormat(), field.getParamater());
			tmpField.setPosLen(1, bytes.length);

			return type.getField(bytes, 1, tmpField);
		} else {
			return layout.formatCsvField(field, typeId, data);
		}
	}



	@Override
	public byte[] getFieldBytes(int recordIdx, int fieldIdx) {
		return null;
	}


	@Override
	public String getFieldText(int recordIdx, int fieldIdx) {
		return  getFieldText(layout.getRecord(recordIdx).getField(fieldIdx));
	}


	private String getFieldText(IFieldDetail fldDef) {

		String s = getFieldRawText(fldDef);
		if (s.isEmpty()) { return s; }
		
		int len = s.length() - 1;
		if (s.charAt(len) == ' ') {
			while (len > 0 && s.charAt(len) == ' ') {
				len -= 1;
			}
			
			s = s.substring(0, len+1);
		}

		return s;
	}
	
	private String getFieldRawText(IFieldDetail fldDef) {
		int start = getFieldsPosition(fldDef) - 1;
		String tData = data;

		int tempLen = fldDef.getLen();
		if (fldDef.getType() == Type.ftCharRestOfRecord || tempLen > tData.length() - start) {
			tempLen = tData.length() - start;
		}

	    if (tData.length() < start || tempLen < 1 || start < 0) {
	        return "";
	    }
		return tData.substring(start, start + tempLen);
	}


	@Override
	public String getFullLine() {
		return data;
	}


	/**
	 * Get the Preferred Record Layout Index for this record
	 *
	 * @return Index of the Record Layout based on the Values
	 */
	@Override
	public int getPreferredLayoutIdx() {
		int ret = preferredLayout;

		if (ret == Constants.NULL_INTEGER) {
			ret = getPreferredLayoutIdxAlt();

			if (ret < 0) {
				for (int i=0; i< layout.getRecordCount(); i++) {
					if (this.data.length() == layout.getRecord(i).getLength()) {
						ret = i;
						preferredLayout = i;
						break;
					}
				}
			}
		}

		return ret;
	}


	@Override
	public void replace(byte[] rec, int start, int len) {

	}
	
	@Override
	public void setData(byte[] newVal) {
		setData(Conversion.toString(newVal, layout.getFontName()));
	}


	@Override
	public void setData(String newVal) {
		data =newVal == null ? "" : newVal;
		clearOdBuffers();
	}



	@Override
	protected void setField(int typeId , IFieldDetail field, Object value) {

		if (field.isFixedFormat()) {
			String s = "";
			Type type = TypeManager.getSystemTypeManager().getType(typeId);
			if (value != null) {
				s = value.toString();
			}

			s = type.formatValueForRecord(field, s);

			int len = field.getLen();
			if (s.length() < len
			&&	type instanceof BaseType && ! ((BaseType) type).isLeftJustified()) {
				s = Conversion.padFront(s, len - s.length(), ' ');
			} 
			updateData(getFieldsPosition(field), len, s);
			
			super.checkForOdUpdate(field);

		} else {
	        ICsvCharLineParser parser = field.getRecord().getCharParser();
	        if (parser.isUpdatable()) {
		        Type typeVal = TypeManager.getSystemTypeManager().getType(typeId);
		        String s = typeVal.formatValueForRecord(field, value.toString());
	
	            data = parser.setField(getFieldsPosition(field) - 1,
	            				typeVal.getFieldType(),
	            				data,
	            				new CsvDefinition(layout.getDelimiterDetails(), field.getQuoteDefinition()), s);
	        }
		}
	}

	@Override
	public String setFieldHex(int recordIdx, int fieldIdx, String val) {
		return null;
	}

	@Override
	public void setFieldText(int recordIdx, int fieldIdx, String value) {
		FieldDetail fldDef = layout.getRecord(recordIdx).getField(fieldIdx);
		
		super.checkForOdUpdate(fldDef);

		updateData(super.getFieldsPosition(fldDef), fldDef.getLen(), value);
	}

	private void updateData(int pos, int length, String value) {
		int i;
		int start = pos -1;
		int len = Math.min(value.length(), length);
		StringBuilder dataBld = new StringBuilder(data);
		int en = start + length - dataBld.length();

		for (i = 0; i < en; i++) {
			dataBld.append(' ');
		}
		for (i = 0; i < len; i++) {
			dataBld.setCharAt(start + i, value.charAt(i));
		}

		for (i = len; i < length; i++) {
			dataBld.setCharAt(start + i, ' ');
		}
		data = dataBld.toString();
	}


    /**
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        Object o = super.clone();
        ((CharLine) o).data = data;
        return lineProvider.getLine(((CharLine) o).layout, ((CharLine) o).data);
    }

    

	@Override
	public boolean isDefined(IFieldDetail field) {
		if (this.data == null || data.length() <= field.getPos() || ! super.isFieldInLine(field) ) {
			return false;
		}

		if (TypeManager.isNumeric(field.getType())) {
			int e = Math.min(field.getPos() + field.getLen(), data.length());
			for (int i = field.getPos() - 1; i < e; i++) {
				switch (data.charAt(i)) {
				case ' ':
				case 0:
					break;
				default:
					return true;
				}
			}
			return false;
		}
		return true;
		
	}


	@Override
	public boolean isValid(IFieldDetail fldDef) {
		return 		super.isFieldInLine(fldDef) 
				&& 	TypeManager.getInstance().getType(fldDef.getType()).isValid(fldDef, getFieldRawText(fldDef));
	}

}
