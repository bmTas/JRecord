package net.sf.JRecord.IO.builders;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import net.sf.JRecord.External.ExternalRecord;

public class CreateExternalFromStream  extends CreateExternalBase  implements ICreateExternal  {

	private final String copybookName;
	private InputStream inStream;
	private byte[] copybookBytes = null ;

	

	public CreateExternalFromStream(CblIOBuilderMultiSchema parent, InputStream inStream, String copybookName) {
		super(parent);
		this.copybookName = copybookName;
		this.inStream = inStream;
	} 

	@Override
	public ExternalRecord createExternalRecordImp() throws Exception {
		
		if (copybookBytes == null) {
			ByteArrayOutputStream os = new ByteArrayOutputStream(0x8000);
			byte[] buf = new byte[0x8000];
			int l = inStream.read(buf);
			while (l > 0) {
				os.write(buf, 0, l);
				l = inStream.read(buf);
			}
			copybookBytes = os.toByteArray();
			inStream = null;
		}

		return parent. loader
				.loadCopyBook(new ByteArrayInputStream(copybookBytes), copybookName, splitCopybook, 0, 
						parent.getFont(), parent.copybookFileFormat, 
						parent.dialect, 0, parent.log);
	}
}
