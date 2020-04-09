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
      
package net.sf.JRecord.Numeric;

import java.util.HashMap;
import java.util.Properties;
import java.util.StringTokenizer;

import net.sf.JRecord.Common.PropertyManager;
import net.sf.JRecord.Types.Type;

public class LoadConversion {

	private final static int[] STANDARD_TYPES = {
		Type.ftFjZonedNumeric,  Type.ftGnuCblZonedNumeric,
		Type.ftBinaryBigEndian, Type.ftFloat, Type.ftDouble, Type.ftPackedDecimal,
		Type.ftBinaryBigEndian, Type.ftBinaryInt
	};
	private static HashMap<String, Integer> typeConversion = new HashMap<String, Integer>();

	static {
		typeConversion.put("be",   Type.ftBinaryBigEndian);
		typeConversion.put("be+",  Type.ftBinaryBigEndianPositive);
		typeConversion.put("le",   Type.ftBinaryInt);
		typeConversion.put("le+",  Type.ftBinaryIntPositive);
		typeConversion.put("pbe",  Type.ftPositiveBinaryBigEndian);
		typeConversion.put("ple",  Type.ftPostiveBinaryInt);
		typeConversion.put("pd",   Type.ftPackedDecimal);
		typeConversion.put("pd+",  Type.ftPackedDecimalPostive);
		typeConversion.put("ppd",  Type.ftDecimal);
		typeConversion.put("f",    Type.ftFloat);
		typeConversion.put("d",    Type.ftDouble);
		typeConversion.put("mvszd",Type.ftZonedNumeric);
//		typeConversion.put("mvszdl",Type.ftZonedLeading);
		typeConversion.put("zd9",  Type.ftGnuCblZonedNumeric);
		typeConversion.put("zd",   Type.ftFjZonedNumeric);
		typeConversion.put("rm",   Type.ftRmComp);
		typeConversion.put("rmp",  Type.ftRmCompPositive);
	}

	private Properties properties = PropertyManager.getProperties();
	private String propConversion = "CnvCode.";

	public Convert getConversion(int idx) {
		Convert ret = null;

		if (properties != null && properties.containsKey(propConversion + idx)) {
			try {
				int code = Integer.parseInt(properties.getProperty(propConversion + idx));
				int binaryId = getIntProperty("CnvBinaryId." + idx, ICopybookDialects.FMT_BIG_ENDIAN);
				int floatSync = getIntProperty("CnvFloatSync." + idx, 4);
				int doubleSync = getIntProperty("CnvDoubleSync." + idx, 8);

				String name = properties.getProperty("CnvName." + idx);
				int[] sizes, syncSizes, syncPos, stdTypes, positiveTypes;

				sizes = getIntArray("CnvSizes." + idx);
				syncSizes = getIntArray("CnvSyncSizes." + idx);
				syncPos = getIntArray("CnvSyncPosition." + idx);

				stdTypes = getTypes("CnvType." + idx, STANDARD_TYPES);
				positiveTypes = getTypes("CnvPositiveType." + idx, stdTypes);

				ret = new GenericNumericDefinition(code, binaryId, name, sizes,
						syncSizes, syncPos, stdTypes,
						positiveTypes, floatSync, doubleSync);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}


		return ret;
	}

	private int getIntProperty(String name, int defaultValue) {
		int ret = defaultValue;
		String s = properties.getProperty(name);

		if (s != null && s.length() > 0) {
			ret = Integer.parseInt(s.trim());
		}

		return ret;
	}

	private int[] getIntArray(String name) {
		int[] ret = null;
		StringTokenizer tok = new StringTokenizer(properties.getProperty(name), ",");
		int count = tok.countTokens();

		if (count > 0) {
			int i = 0;
			ret = new int[count];
			String s = "";
			while (tok.hasMoreElements()) {
				ret[i] = -1;
				try {
					s = tok.nextToken();
					ret[i] = Integer.parseInt(s.trim());
				} catch (Exception e) {
					System.out.println("Invalid Integer :" + s + ", in " + name);
				}
				i+= 1;
			}
		}
		return ret;
	}

	private int[] getTypes(String name, int[] base) {
		int[] ret = base;

		try {
			StringTokenizer tok = new StringTokenizer(properties.getProperty(name), ",");
			int count = tok.countTokens();

			if (count > 0) {
				int i = 0;
				ret = new int[Math.max(count, base.length)];
				String s = "";

				for (int j = 0; j < base.length; j++) {
					ret[j] = base[j];
				}

				while (tok.hasMoreElements()) {
					s = tok.nextToken();

					if (s != null && typeConversion.containsKey(s.toLowerCase())) {
						ret[i] = typeConversion.get(s.toLowerCase());
					}

					i+= 1;
				}
			}
		} catch (Exception e) {
			// TODO: handle exception
		}
		return ret;

	}
}
