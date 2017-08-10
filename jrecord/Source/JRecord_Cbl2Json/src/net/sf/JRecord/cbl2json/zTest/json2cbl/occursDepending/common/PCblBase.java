package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;


public abstract class PCblBase implements IProcessCobolTree {

	public PCblBase() {
	}

	@Override
	public void intField(String name, int value, ArrayIndex indexs) {

	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#intArrayField(java.lang.String, int, net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ArrayIndex)
	 */
	@Override
	public void intArrayField(String name, int value, ArrayIndex indexs) {
		intField(name, value, indexs);
	}

	@Override
	public void startGroup(String name, ArrayIndex indexs) {

	}

	@Override
	public void endGroup(String name) {
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#startArrayItem(java.lang.String, net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ArrayIndex)
	 */
	@Override
	public void startArrayItem(String name, ArrayIndex indexs) {
		startGroup(name, indexs);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.IProcessCobolTree#endArrayItem(java.lang.String)
	 */
	@Override
	public void endArrayItem(String name) {
		endGroup(name);
	}

	@Override
	public void startArray(String name, ArrayIndex indexs) {

	}

	@Override
	public void endArray(String name) {

	}
}
