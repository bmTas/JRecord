package net.sf.JRecord.Common;

import java.io.File;
import java.io.FileInputStream;
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
        System.out.println("Properties File: "
               + name
               + " " + f.exists());
        if (f.exists()) {

            try {
                ret.load(new FileInputStream(f));
            } catch (Exception e) {
                //e.printStackTrace();
            }
        }

	    return ret;
	}


}
