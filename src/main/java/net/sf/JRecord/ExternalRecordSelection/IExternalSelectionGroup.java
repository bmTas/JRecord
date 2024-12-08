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
 * ExternalSelection has 2 extended interfaces:<ul>
 * <li><b>IExternalSelectionGroup</b> implements boolean and/or operators.
 * There can be multiple <i>child</i> ExternalSelection's.
 * <li><B>IExternalSelectionField</b> which tests one field. These have no 
 * child ExternalSelection's.
 * </ul>
 *     
 * @author Bruce Martin
 *
 */
public interface IExternalSelectionGroup extends ExternalSelection {
	/**
	 * 
	 * @param index index of a child selection
	 * @return requested child selection
	 */
	public ExternalSelection get(int index);
}
