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
      
package net.sf.JRecord.Common;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;


public class PropertyManager {
	
	private static final int SIZE_OF_FILE_URL_PREFIX = 9;
	
	private static Properties properties = null;
	

    public static Properties getProperties() {
    	if (properties == null) {
    		properties = readProperties(getDirectory() + "/JRecord.properties");
    	}
		return properties;
	}


	private static String getDirectory() {


        String baseDirectory = "/home/bm/RecordEdit/HSQLDB/lib";

        URL o = PropertyManager.class.getClassLoader().getResource("net/sf/JRecord/Common/PropertyManager.class");

        if (o != null) {
            String dir = o.toString();

            if (dir.startsWith("jar:")) {
                int pos = dir.indexOf('!');
                
                baseDirectory = dir.substring(SIZE_OF_FILE_URL_PREFIX, pos);
                pos = baseDirectory.lastIndexOf('/');
                if (pos >= 0) {
                    baseDirectory = baseDirectory.substring(0, pos);
                }
            }
        }
        
//        System.out.println("Get Jrecord porperties Dir: " + o + " " + baseDirectory);
        return baseDirectory;
    }

	/**
	 * read the properties file
	 *
	 * @param name property filename
	 *
	 * @return properties file
	 */
	public static Properties readProperties(String name) {
	    Properties ret = new Properties();

        File f = new File(name);
//        System.out.println("Properties File: "
//               + name
//               + " " + f.exists());
        if (f.exists()) {
			FileInputStream fs = null;
            try {
            	fs = new FileInputStream(f);
                ret.load(fs);
            } catch (Exception e) {

            } finally {
				if (fs != null) {
					try {
						fs.close();
					} catch (IOException e) {
					}
				}
			}
        }

	    return ret;
	}


}
