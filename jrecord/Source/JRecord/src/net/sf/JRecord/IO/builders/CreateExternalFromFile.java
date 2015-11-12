package net.sf.JRecord.IO.builders;

import net.sf.JRecord.External.ExternalRecord;

public class CreateExternalFromFile extends CreateExternalBase implements ICreateExternal  {

	private final String copybookFilename;
//	private final CblIOBuilderMultiSchema parent;

	

	public CreateExternalFromFile(CblIOBuilderMultiSchema parent, String copybookFilename) {
		super(parent); 
		this.copybookFilename = copybookFilename;
	} 


	@Override
	public ExternalRecord createExternalRecordImp() throws Exception {
		return parent.loader.loadCopyBook(copybookFilename, splitCopybook, 0, parent.getFont(), parent.copybookFileFormat, parent.dialect, 0, parent.log);
	}
}
