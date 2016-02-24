package net.sf.JRecord.IO.builders;

import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.ISetDropCopybookName;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public abstract class CreateExternalBase {
	final IGetLoader parent;

	int splitCopybook = CopybookLoader.SPLIT_NONE;
	private ExternalSelection recordSelection = null;

	protected CreateExternalBase(IGetLoader parent) {
		super();
		this.parent = parent;
	}

	
	/**
	 * @param split the split to set
	 */
	public final void setSplitCopybook(int split) {
		this.splitCopybook = split;
		doCheck();
	}
	
	/**
	 * @param recordSelection the recordSelection to set
	 */
	public final void setRecordSelection(ExternalSelection recordSelection) {
		this.recordSelection = recordSelection;
		doCheck();
	}
	
	private void doCheck() {
		if (recordSelection != null && splitCopybook != CopybookLoader.SPLIT_NONE) {
			throw new RuntimeException("A record selection can only be specified when split=None");
		}
	}
	

	public ExternalRecord createExternalRecord() throws Exception {
		
		ICopybookLoaderStream loader = parent.getLoader();
		if (loader instanceof ISetDropCopybookName) { 
			((ISetDropCopybookName) loader).setDropCopybookFromFieldNames(parent.isDropCopybookNameFromFields());
		}

		ExternalRecord r = createExternalRecordImp();
		
		if (recordSelection != null) {
			r.setRecordSelection(recordSelection);
		}
		
		return r;
	}
	
	public abstract ExternalRecord createExternalRecordImp() throws Exception;

}
