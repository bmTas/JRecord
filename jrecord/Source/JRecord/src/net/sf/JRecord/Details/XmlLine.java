/*
 * @Author Bruce Martin
 * Created on 20/04/2007
 *
 * Purpose:
 */
package net.sf.JRecord.Details;


/**
 * Line for use with XML files.
 *
 * <p>The one important method is getFieldValue
 *
 * <p>Creating:
 * <pre>
 *              AbstractLine outLine = <font color="brown"><b>new</b></font> XmlLine(oLayout, recordIdx);
 * </pre>
 *
 * <p>Getting a field value:
 * <pre>
 *              <font color="brown"><b>long</b></font> sku = saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").asLong();
 * </pre>
 *
 * <p>Updating a field:
 * <pre>
 *              saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").set(1331);
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class XmlLine extends ListLine {

    

	//private static LineProvider defaultProvider = new DefaultLineProvider();

	//private LayoutDetail layout;

	private boolean useField4Index = true;

	public XmlLine(LayoutDetail layoutDetails, int recordIdx) {
		super(layoutDetails);
	    layout = layoutDetails;
	    newRecord = recordIdx < 0;
	    preferredLayout = recordIdx;

	    fields.add("");

	    if (newRecord) {
	    	fields.add("True");
	    }
	}
	

	/**
	 * @see net.sf.JRecord.Details.AbstractLine#getFullLine()
	 */
	public String getFullLine() {
	    return "";
	}



    protected int getFieldNumber(int recordIdx, int fieldIdx) {
       	int idx = fieldIdx;

        //   	System.out.print("getField " + recordIdx + " " + fieldIdx);
           	if (useField4Index && recordIdx < layout.getRecordCount()
           	&& fieldIdx < layout.getRecord(recordIdx).getFieldCount()
           	&& layout.getRecord(recordIdx).getField(fieldIdx).getPos() >= 0) {
           		idx = layout.getRecord(recordIdx).getField(fieldIdx).getPos();
           	}
           	return idx;
    }

   /**
     * Test if Tree rebuild is required
     */
	public boolean isRebuildTreeRequired() {
		boolean ret = rebuildRequired;
		return ret;
	}


	/**
	 * @param useField4Index the useField4Index to set
	 */
	public final void setUseField4Index(boolean useField4Index) {
		this.useField4Index = useField4Index;
	}
	

	@Override
	protected final int getAdj() {
		return 0;
	}

}
