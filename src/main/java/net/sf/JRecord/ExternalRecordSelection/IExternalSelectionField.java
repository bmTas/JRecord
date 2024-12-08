package net.sf.JRecord.ExternalRecordSelection;

/**
 * An ExternalSelection is boolean expression stored as a tree i.e.
 * 
 *                      +--- Field Test 1
 *                      |     
 *        +------and----+--- Field Test 2
 *        |             |     
 * ---or--+             +--- Field Test 3
 *        |                 
 *        +-------------+--- Field Test 3
 *        
 *        
 * ExternalSelection has 2 extended interfaces:<ul>
 * <li><b>IExternalSelectionGroup</b> implements boolean and/or operators.
 * There can be multiple <i>child</i> ExternalSelection's.
 * <li><B>IExternalSelectionField</b> which tests one field. These have no 
 * child ExternalSelection's.
 * </ul>
 *        
 *   Together these clases form complete boolean expressions
 *   
 *   
 * @author Bruce Martin
 *
 */
public interface IExternalSelectionField extends ExternalSelection {

	/**
	 * 
	 * @return Name of the field being tested
	 */
	public String getFieldName();
	/**
	 * 
	 * @return operator being used to to test the field (i.e. = > < etc)
	 */
	public String getOperator();
	/**
	 * 
	 * @return value the field is being tested against
	 */
	public String getFieldValue();
	
}
