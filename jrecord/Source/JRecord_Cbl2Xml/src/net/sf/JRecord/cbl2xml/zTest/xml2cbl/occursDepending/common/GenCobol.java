package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

public class GenCobol {

	private static final GenCobol INSTANCE = new GenCobol();
	public static final String SPACES = "                                                                  "
									  + "                                                                  "
									  + "                                                                  ";

	public static String genCobol(CblItem cblItem) {
		StringBuilder b = new StringBuilder("       01  " + cblItem.name +".\n");
		
		INSTANCE.genCobolLevel(b, cblItem, 3);
		
		return b.toString();
	}
	
	public void genCobolLevel(StringBuilder b, CblItem cblItem, int level) {
		String pref = "        " + SPACES.substring(SPACES.length() - level)
					+ level + " " ;
		
		for (CblItem c : cblItem.children) {
			switch (c.itemType) {
			case CblItem.INT_FIELD:
//			case CblItem.SIZE_FIELD:
				String name = c.name;
				genIntField(b, pref, name);
				break;
			case CblItem.GROUP:
				b.append(pref).append(c.name).append(".\n");
				genCobolLevel(b, c, level + 2);
				break;
			case CblItem.INT_ARRAY_FIELD:
				b.append(pref).append(c.name).append(' ')
				 .append("occurs ")
				 .append(c.arraySize)
				 .append("\n")
				 .append(SPACES.substring(SPACES.length() - 50))
				 .append("pic 9(6).\n");
				break;
			case CblItem.INT_OD_ARRAY_FIELD:
				genIntField(b, pref, c.dependinOn);
				b.append(pref).append(c.name).append(' ')
				 .append("occurs 0 to ")
				 .append(c.arraySize)
				 .append(" depending on ")
				 .append(c.dependinOn)
				 .append("\n")
				 .append(SPACES.substring(SPACES.length() - 50))
				 .append("pic 9(6).\n");
				break;
			case CblItem.ARRAY:
				b.append(pref).append(c.name).append(' ')
				 .append("occurs ")
				 .append(c.arraySize)
				 .append(".\n");
				genCobolLevel(b, c, level + 2);
				break;
			case CblItem.OD_ARRAY:
				genIntField(b, pref, c.dependinOn);
				b.append(pref).append(c.name).append(' ')
				 .append("occurs 0 to ")
				 .append(c.arraySize)
				 .append(" depending on ")
				 .append(c.dependinOn)
				 .append(".\n");
				genCobolLevel(b, c, level + 2);
			}
		}
	}

	/**
	 * @param b
	 * @param pref
	 * @param name
	 */
	public void genIntField(StringBuilder b, String pref, String name) {
		String s = pref + name;
		b.append(s);
		if (s.length() >= 50) {
			b.append('\n');
			s = "";
		}
		b.append(SPACES.substring(SPACES.length() - 50 + s.length()))
		 .append("pic 9(6).\n");
	}

}
