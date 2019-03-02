package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;

public class PCblGenXml extends PCblBase {

	private StringBuilder xmlBldr = new StringBuilder(1000)
								  .append("<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData>");
	private boolean addEnd = true;
	
	public String getXml() {
		if (addEnd) {
			xmlBldr.append("</CobolData>");
			addEnd = false;
		}
		return xmlBldr.toString();
	}


	@Override
	public void intField(String name, int value, ArrayIndex indexs) {
		genXmlTag(name);
		xmlBldr.append(value);
		genXmlEndTag(name);
	}



	/**
	 * @param name
	 * @return
	 */
	public void genXmlTag(String name) {
		xmlBldr.append('<')
			   .append(name)
			   .append('>');
	}


	/**
	 * @param name
	 */
	public void genXmlEndTag(String name) {
		xmlBldr.append("</")
			   .append(name)
			   .append('>');
	}

	@Override
	public void startGroup(String name, ArrayIndex indexs) {
		genXmlTag(name);
	}

	@Override
	public void endGroup(String name) {
		genXmlEndTag(name);
	}

	@Override
	public int getCount(CblItem c, ArrayIndex indexs, ArrayIndex countIndexs) {
		return c.getCount(indexs, countIndexs);
	}

}
