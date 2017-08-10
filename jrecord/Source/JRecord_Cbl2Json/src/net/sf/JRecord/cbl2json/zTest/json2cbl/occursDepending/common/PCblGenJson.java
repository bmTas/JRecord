package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;

import java.util.ArrayList;

public class PCblGenJson implements IProcessCobolTree {

	private StringBuilder xmlBldr = new StringBuilder(1000)
								  .append("{");
	private boolean addEnd = true;
	private StrStack sep = new StrStack();
	
	public String getXml() {
		if (addEnd) {
			xmlBldr.append("}");
			addEnd = false;
		}
		return xmlBldr.toString();
	}


	@Override
	public void intField(String name, int value, ArrayIndex indexs) {

		xmlBldr	.append(sep.get())
				.append("\"")
				.append(name)
				.append("\":")
				.append(value);
		sep.set(",");
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#intArrayField(java.lang.String, int, net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ArrayIndex)
	 */
	@Override
	public void intArrayField(String name, int value, ArrayIndex indexs) {
		xmlBldr	.append(sep.get())
				.append(value);
		sep.set(",");
	}


//	/**
//	 * @param name
//	 * @return
//	 */
//	public void genXmlTag(String name) {
//		xmlBldr.append('<')
//			   .append(name)
//			   .append('>');
//	}
//
//
//	/**
//	 * @param name
//	 */
//	public void genXmlEndTag(String name) {
//		xmlBldr.append("</")
//			   .append(name)
//			   .append('>');
//	}

	@Override
	public void startGroup(String name, ArrayIndex indexs) {
		xmlBldr	.append(sep.get())
				.append("\"")
				.append(name)
				.append("\":{");
		sep.push("");
	}

	@Override
	public void endGroup(String name) {
		xmlBldr	.append("}");
		sep.pop();
		sep.set(",");
	}

	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#startArrayItem(java.lang.String, net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ArrayIndex)
	 */
	@Override
	public void startArrayItem(String name, ArrayIndex indexs) {
		xmlBldr	.append(sep.get())
				.append("{");
		sep.push("");
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#endArrayItem(java.lang.String)
	 */
	@Override
	public void endArrayItem(String name) {
		endGroup(name);
		//sep.set(",");
	}


	@Override
	public void startArray(String name, ArrayIndex indexs) {
		xmlBldr	.append(sep.get())
				.append("\"")
				.append(name)
				.append("\":[");
		sep.push("");
	}

	@Override
	public void endArray(String name) {
		sep.pop();
		xmlBldr	.append("]");
	}

	@Override
	public int getCount(CblItem c, ArrayIndex indexs, ArrayIndex countIndexs) {
		return c.getCount(indexs, countIndexs);
	}

	private static class StrStack {
		ArrayList<String> stack = new ArrayList<String>();
		
		public StrStack() {
			stack.add("");
		}
		
		public void set(String s) {
			stack.set(stack.size() - 1, s);
		}
		
		public String get() {
			return stack.get(stack.size() - 1);
		}
		
		public void pop() {
			stack.remove(stack.size() - 1);
		}
		
		public void push(String s) {
			stack.add(s);
		}
	}
}
