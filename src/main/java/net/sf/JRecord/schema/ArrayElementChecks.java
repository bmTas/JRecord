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

package net.sf.JRecord.schema;

import java.util.Arrays;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.schema.jaxb.IItem;


/**
 * Class holds various Array-Check Creator's (or Builders).
 * Array checks (interface: IArrayItemCheck) will check wether an
 * <i>Array-Item</i> should be converted to Xml / JSon etc. 
 * When Converting an Xml/Json file to Cobol, it can do updates after
 * the last <i>Array-Item</i> (i.e set sizes etc).
 * 
 * @author Bruce Martin 
 *
 */
public class ArrayElementChecks {
	
	public static final ArrayElementChecks INSTANCE = new ArrayElementChecks();
	

	private static final IArrayItemCheck checkForSpaces = new SpaceCheck(IArrayItemCheck.R_SKIP, false); 
	private static final IArrayItemCheck stopAtSpaces   = new SpaceCheck(IArrayItemCheck.R_STOP, false); 
	
	private static final IArrayItemCheck checkForSpacesZeros = new SpaceCheck(IArrayItemCheck.R_SKIP, true); 
	private static final IArrayItemCheck stopAtSpacesZeros   = new SpaceCheck(IArrayItemCheck.R_STOP, true); 
	
	/**
	 * Class to skip array item (if low values (Hex Zero's - x00))
	 */
	private static final IArrayItemCheck checkForLowValues  = new Check4byte((byte) 0, IArrayItemCheck.R_SKIP);
	private static final IArrayItemCheck stopAtLowValues    = new Check4byte((byte) 0, IArrayItemCheck.R_STOP);
	/**
	 * Class to skip array item (if high values (0xFF))
	 */
	private static final IArrayItemCheck checkForHighValues = new Check4byte((byte) 0xFF, IArrayItemCheck.R_SKIP);
	private static final IArrayItemCheck stopAtHighValues   = new Check4byte((byte) 0xFF, IArrayItemCheck.R_STOP);
	
	/**
	 * Create item to check an array index
	 * @param varName name of the array index field.
	 * @return Array-Check
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newIndexCheck(String varName) {
		return new CheckIndex(varName);
	}
	
	/**
	 * @return Item to Skip array item if it is spaces
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newSkipSpaces() {
		return checkForSpaces;
	}
	
	/**
	 * @return Item to Stop array processing when the index is spaces
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newStopAtSpaces() {
		return stopAtSpaces;
	}
	/**
	 * @return Item to Skip array item if it is spaces
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newSkipSpacesZeros() {
		return checkForSpacesZeros;
	}
	
	/**
	 * @return Item to Stop array processing when the index is spaces
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newStopAtSpacesZeros() {
		return stopAtSpacesZeros;
	}

	/**
	 * @return Item to Skip array item if it is High-Values (x'FF')
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newSkipHighValues() {
		return checkForHighValues;
	}
	
	
	/**
	 * @return Item to Stop array processing at the first item that is High-Values (x'FF')
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newStopAtHighValues() {
		return stopAtHighValues;
	}
	
	
	/**
	 * @return Item to Skip array item if it is low-Values (x'00')
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newSkipLowValues() {
		return checkForLowValues;
	}
	
	
	/**
	 * @return Item to Stop array processing at the first item that is low-Values (x'00')
	 * 
	 * <b>Note:</b> Only single dimension arrays are currently supported
	 */
	public IArrayItemCheck newStopAtLowValues() {
		return stopAtLowValues;
	}
	
	
	private static class Check4byte implements IArrayItemCheck {
		private final byte checkByte;
		private final int result;
		
		protected Check4byte(byte checkByte, int result) {
			super();
			this.checkByte = checkByte;
			this.result = result;
		}

		@Override
		public int checkItem(AbstractLine line, IItem item, int[] indexs, int index) {
			int length = item.getStorageLength();
			int pos = item.getPosition(indexs) + length * index - 1;
			if (line instanceof Line) {
				byte[] bytes = ((Line) line).getData();
				for (int i = 0; i < length; i++) {
					if (bytes[pos + i] != checkByte) {
						return IArrayItemCheck.R_PROCESS; //IArrayItemCheck.R_PROCESS;
					}
				}
				return result;
			}
			return IArrayItemCheck.R_PROCESS;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.schema.IArrayItemCheck#getCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public int getCount(AbstractLine line, IItem item, int[] indexs, int defaultCount) {
			return defaultCount;
		}

		/**
		 * @see net.sf.JRecord.schema.IArrayItemCheck#updateForCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public void updateForCount(AbstractLine line, IItem item, int[] indexs, int count) {
			Integer occurs = item.getOccurs();
			if (line instanceof Line && occurs != null && occurs > count) {
				int length = item.getStorageLength();
				int pos = item.getPosition(indexs) + length * count - 1;
				int end  = (occurs - count) * length; 
				byte[] bytes = ((Line) line).getData();
				Arrays.fill(bytes, pos, pos + end, checkByte);
			}
		}		
	}
	
	
	//TODO
	public static class SpaceCheck implements IArrayItemCheck {
		private final int result;
		private final boolean checkZero;
		
		protected SpaceCheck(int result, boolean checkZero) {
			super();
			this.result = result;
			this.checkZero = checkZero;
		}
		
		@Override
		public int checkItem(AbstractLine line, IItem item, int[] indexs, int index) {
			int length = item.getStorageLength();
			int pos = item.getPosition(indexs) + length * index - 1;
			int[] xxx = new int[indexs.length + 1];
			System.arraycopy(indexs, 0, xxx, 0, indexs.length);
			xxx[indexs.length] = index;
			
//			System.out.println("~~ " + indexs.length + " ~ " + item.getPosition(xxx) + " " + item.getPosition(indexs) + " " + (length * index - 1));
			if (line instanceof Line) {
				byte[] bytes = ((Line) line).getData();
				byte spaceByte = line.getLayout().getSpaceByte();
				byte zeroByte = Conversion.getBytes("0", line.getLayout().getFontName())[0];
				for (int i = 0; i < length; i++) {
					if (bytes[pos + i] == spaceByte
					|| (checkZero && bytes[pos + i] == zeroByte)) {
						
					} else {
//						System.out.println("+++ > " + i + ">" + line.getFullLine().substring(pos, pos + length) + "<");
						return IArrayItemCheck.R_PROCESS;
					}
				}
			} else {
				String s = line.getFullLine();
				for (int i = 0; i < length; i++) {
					switch (s.charAt(pos + i)) {
					case ' ':		break;
					case '0':
						if (checkZero) {
							break;
						}
					default:
						return IArrayItemCheck.R_PROCESS;
					}
				}
			}
			return result;
		}
		
		/* (non-Javadoc)
		 * @see net.sf.JRecord.schema.IArrayItemCheck#getCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public int getCount(AbstractLine line, IItem item, int[] indexs, int defaultCount) {
			return defaultCount;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.schema.IArrayItemCheck#updateForCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public void updateForCount(AbstractLine line, IItem item, int[] indexs, int count) {
			Integer occurs = item.getOccurs();
			if (occurs == null || occurs <= count) {
			} else {
				int length = item.getStorageLength();
				int pos = item.getPosition(indexs) + length * count - 1;
				int end  = (occurs - count) * length; 
				if (line instanceof Line && occurs > count) {		
					byte spaceByte = line.getLayout().getSpaceByte();
					byte[] bytes = ((Line) line).getData();
					Arrays.fill(bytes, pos, Math.min(bytes.length, pos + end), spaceByte);
				} else if (line instanceof CharLine){
					StringBuilder b = new StringBuilder(line.getFullLine());
					for (int i = pos; i < pos + end; i++) {
						b.setCharAt(i, ' ');
					}
					line.setData(b.toString());
				}
			}
		}
	
	}
	
	
	private static class CheckIndex implements IArrayItemCheck {
		private final String varName;
		private int lastIdx = 0;
		
		protected CheckIndex(String varName) {
			this.varName = varName;
		}

		@Override
		public int checkItem(AbstractLine line, IItem item, int[] indexs, int index) {
			
			lastIdx = line.getFieldValue(varName).asInt();
			
			if (index >= lastIdx) {
				return IArrayItemCheck.R_STOP;
			}
			return IArrayItemCheck.R_PROCESS;
		}
		
		/* (non-Javadoc)
		 * @see net.sf.JRecord.schema.IArrayItemCheck#getCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public int getCount(AbstractLine line, IItem item, int[] indexs, int defaultCount) {
			lastIdx = line.getFieldValue(varName).asInt();
			return Math.min(lastIdx, defaultCount);
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.schema.IArrayItemCheck#updateForCount(net.sf.JRecord.Details.AbstractLine, int)
		 */
		@Override
		public void updateForCount(AbstractLine line, IItem item, int[] indexs, int count) {
			line.getFieldValue(varName).set(count);
		}
	}

}
