package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;

import net.sf.JRecord.Details.AbstractLine;

public class ProcessCobolTree {

	public static final String SPACES = "                                                                  "
									  + "                                                                  "
									  + "                                                                  ";
	private final ArrayIndex countIndexs;
	private final ArrayIndex indexs = new ArrayIndex();
	private final IProcessCobolTree processCobol;
	

	protected ProcessCobolTree(ArrayIndex countIndexs, IProcessCobolTree processCobol) {
		super();
		this.countIndexs = countIndexs;
		this.processCobol = processCobol;
	}
	
	/**
	 * Generate a Cobol copybook from a CobolTree ({@link CblItem})
	 * @param cblItem Cobol tree to generate the Cobol.
	 * @return Cobol Copybook
	 */
	public static String generateCobol(CblItem cblItem) {
		return GenCobol.genCobol(cblItem);
	}

	public static void processCobol(CblItem cblItem, IProcessCobolTree processCobol, ArrayIndex countIndexs) {
		new ProcessCobolTree(countIndexs, processCobol)
			.processCobol01Level(cblItem);
	}
	
	/**
	 * Generate a JRecord Line from a CobolTree ({@link CblItem})
	 * @param line
	 * @param cblItem
	 * @param countIndexs
	 * @return
	 */
	public static AbstractLine generateLine(AbstractLine line, CblItem cblItem, ArrayIndex countIndexs) {
		processCobol(cblItem, new PCblUpdateLine(line), countIndexs);
		
		return line;
	}
	
	/**
	 * Generate Cobol for a CobolTree ({@link CblItem})
	 * @param cblItem
	 * @param countIndexs
	 * @return
	 */
	public static String generateXml(CblItem cblItem, ArrayIndex countIndexs) {
		PCblGenXml genXml = new PCblGenXml();
		processCobol(cblItem, genXml, countIndexs);
		
		return genXml.getXml();
	}
	
	public static String generateJson(CblItem cblItem, ArrayIndex countIndexs) {
		PCblGenJson genXml = new PCblGenJson();
		processCobol(cblItem, genXml, countIndexs);
		
		return genXml.getXml();
	}

	/**
	 * This will test a simple Cobol Tree of the form
	 * <pre>
	 *     01 Record.
	 *        03 count1     pic 999.
	 *        03 a1 occurs 1 to 5 depending on count1.
	 *           ....
	 *           
	 * or 
	 *     01 Record.
	 *        03 count1     pic 999.
	 *        03 a1 occurs 1 to 5 depending on count1.
	 *           05 count11     pic 999.
	 *           05 a11 occurs 1 to 5 depending on count11.
	 *              ....
	 * etc
	 * </pre>
	 * 
	 * i.e. as many levels as desired but only one array at each Level.
	 * 
	 * @param cblItem Cobol Item Tree
	 */
	public static void testConversion(CblItem cblItem) {
		processCobol(cblItem, new PCblDoTest(cblItem), new ArrayIndex());
	}

	public void processCobol01Level(CblItem cblItem) {
		processCobol.startArray(cblItem.name, indexs);
		processCobol.startArrayItem(cblItem.name, countIndexs);
		processCobolLevel(cblItem);
		processCobol.endArrayItem(cblItem.name);
		processCobol.endArray(cblItem.name);
	}
	
	public void processCobolLevel(CblItem cblItem) {
		int value = indexs.getValue(10) + 1;
		int count;
		
		for (CblItem c : cblItem.children) {
			switch (c.itemType) {
			case CblItem.INT_FIELD:
//			case CblItem.SIZE_FIELD:
				processCobol.intField(c.name, value++, indexs);
				break;
			case CblItem.GROUP:
				processCobol.startGroup(c.name, indexs);
				processCobolLevel(c);
				processCobol.endGroup(c.name);
				break;
			case CblItem.INT_ARRAY_FIELD:
			case CblItem.INT_OD_ARRAY_FIELD:
				count = processCount(c);
				if (count > 0) {
					processCobol.startArray(c.name, indexs);
					for (int i = 0; i < count; i++) {
						processCobol.intArrayField(c.name, value++, indexs);
						indexs.incIndex();
					}
					processCobol.endArray(c.name);
				}
				indexs.decNumberOfIndexs();
				break;
			case CblItem.ARRAY:
			case CblItem.OD_ARRAY:
				count = processCount(c);
				if (count > 0) {
					processCobol.startArray(c.name, indexs);
					for (int i = 0; i < count; i++) {
						processCobol.startArrayItem(c.name, indexs);
						processCobolLevel(c);
						processCobol.endArrayItem(c.name);
						indexs.incIndex();
					}
					processCobol.endArray(c.name);
				}
				indexs.decNumberOfIndexs();
			}
		}
	}

	/**
	 * @param c
	 * @return
	 */
	public int processCount(CblItem c) {
		int count;
		count = processCobol.getCount(c, indexs, countIndexs);
		if (c.dependinOn != null && c.dependinOn.length() > 0) {
			processCobol.intField(c.dependinOn, count, indexs);
		}
		indexs.incNumberOfIndexs();
		return count;
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
