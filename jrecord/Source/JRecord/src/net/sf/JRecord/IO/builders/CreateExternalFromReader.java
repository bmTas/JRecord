package net.sf.JRecord.IO.builders;

import java.io.Reader;
import java.io.StringReader;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;

public class CreateExternalFromReader  extends CreateExternalBase  implements ICreateExternal  {

	private final String copybookName;
	private Reader reader;
	private String copybookStr = null ;

	

	public CreateExternalFromReader(IGetLoader parent, Reader reader, String copybookName) {
		super(parent);
		this.copybookName = copybookName;
		this.reader = reader;
	} 

	@Override
	public ExternalRecord createExternalRecordImp() throws Exception {
		
		if (copybookStr == null) {
			char[] buf = new char[0x8000];
			int l = reader.read(buf);
			StringBuilder copybookSB = new StringBuilder();
			while (l > 0) {
				copybookSB.append(buf, 0, l);
				l = reader.read(buf);
			}
			reader = null;
			copybookStr = copybookSB.toString();
		}

		
		ICopybookLoaderStream loader = parent. getLoader();
		
		if (loader instanceof net.sf.JRecord.External.CobolCopybookLoader) {
			return ((net.sf.JRecord.External.CobolCopybookLoader)loader)
				.loadCopyBook(new StringReader(copybookStr), copybookName, splitCopybook, 0, 
						parent.getFont(), parent.getCopybookFileFormat(), 
						parent.getDialect(), 0, parent.getLog());
		}
		throw new RecordException("Internal Error: loader is not a Cobol Copybook loader");
	}
}
