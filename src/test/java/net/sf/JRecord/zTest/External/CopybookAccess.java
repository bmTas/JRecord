package net.sf.JRecord.zTest.External;

import java.net.URI;
import java.net.URISyntaxException;

import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;

public class CopybookAccess {


	public static String getDTAR020BinFileName() {
		return toFileName("DTAR020_tst1.bin");
	}
	//Ams_PODownload_tst.txt
	
	public static String getAmsTextFileName() {
		return toFileName("Ams_PODownload_tst.txt");
	}
	
	public static ExternalRecord getAmsPo() throws Exception {
		return getCobolRecord("amsPoDownload.xml");
	}


	public static ExternalRecord getAmsPo(int id) throws Exception {
		return getCobolRecord("amsPoDownload" + id + ".xml");
	}

	
	public static ExternalRecord getXmlAmsPo() throws Exception {
		return getXmlRecord("ams PO Download.Xml");
	}

	
	public static ExternalRecord getXmlAmsPo2() throws Exception {
		return getXmlRecord("ams PO Download2.Xml");
	}

	private static ExternalRecord getCobolRecord(String cobolFileName) throws Exception {
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();

		ExternalRecord amsPoRecord =  loader.loadCopyBook(toFileName(cobolFileName), 0, 0, "", 0, 0, new TextLog());
				
//		amsPoRecord.setFileStructure(IFileStructureConstants.IO_BIN_TEXT);
		
//		for (int i = 0; i < amsPoRecord.getChildRecordCount(); i++) {
//			System.out.println(amsPoRecord.getChildRecord(i).getExternalRecord().getRecordName());
//		}

		return amsPoRecord;
	}

	private static ExternalRecord getXmlRecord(String xmlFileName) throws Exception {
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();

		ExternalRecord amsPoRecord =  loader.loadCopyBook(toXmlFileName(xmlFileName), 0, 0, "", 0, 0, new TextLog());
				
		return amsPoRecord;
	}

	public static ExternalRecord getDTAR020() throws Exception {
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();
		
		ExternalRecord dtar020record = loader.loadCopyBook(toFileName("DTAR020.xml"), 0, 0, "", 0, 0, new TextLog());
		
		return dtar020record;
	}

	public static ExternalRecord getDTAR020a() throws Exception {
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();
		
		ExternalRecord dtar020record = loader.loadCopyBook(toFileName("DTAR020a.xml"), 0, 0, "", 0, 0, new TextLog());
		
		return dtar020record;
	}


//	private static InputStream toStream(String fileName) {
//		return CobolAccess.class.getResourceAsStream(fileName);
//	}
	
	private static String toFileName(String fileName) {
		String name = "cobol/" + fileName;
//		System.out.println(name);

		return fullFileName(name);
	}
	
	private static String toXmlFileName(String fileName) {
		String name = "recordEditorXml/" + fileName;
//		System.out.println(name);

		return fullFileName(name);
	}
	private static String fullFileName(String relativePath) {
		URI uri;
		try {
			uri = CopybookAccess.class.getResource(relativePath).toURI();
		} catch (URISyntaxException e) {
			e.printStackTrace();
			return "";
		}
		return uri.getPath();
		
	}
}
