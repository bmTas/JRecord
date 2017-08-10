package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;

/**
 * This class 
 * @author Bruce01
 *
 */
public interface IProcessCobolTree {

	public void intField(String name, int value, ArrayIndex indexs);
	public void intArrayField(String name, int value, ArrayIndex indexs);
	
	public void startGroup(String name, ArrayIndex indexs);
	public void endGroup(String name);
	public void startArrayItem(String name, ArrayIndex indexs);
	public void endArrayItem(String name);

	public void startArray(String name, ArrayIndex indexs);
	
	public void endArray(String name);
	
	public int getCount(CblItem c, ArrayIndex indexs, ArrayIndex countIndexs);

}
