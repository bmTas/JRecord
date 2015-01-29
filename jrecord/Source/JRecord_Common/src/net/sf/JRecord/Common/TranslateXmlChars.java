package net.sf.JRecord.Common;

public class TranslateXmlChars {

	public static String replaceXmlCharsStr(String in) {
		return replaceXmlChars(new StringBuilder(in)).toString();
	}
	
	public static StringBuilder replaceXmlChars(StringBuilder in) {
        replace(in, "&", "&amp;");
        replace(in, "<", "&lt;");
        replace(in, ">", "&gt;");
        replace(in, "\"", "&quot;");
 //       replace(in, "\t", "&#009;");
        
        return in;
	}
	
	/**
     * Replaces on string with another in a String bugffer
     *
     * @param in String buffer to be updated
     * @param from search string 
     * @param to replacement string
     */
    public static void replace(StringBuilder in, String from, String to) {
        int start;
        int fromLen = from.length();

        start = in.indexOf(from, 0);
        while (start >= 0) {
            in.replace(start, start + fromLen, to);
            start = in.indexOf(from, start + to.length());
        }
    }
}
