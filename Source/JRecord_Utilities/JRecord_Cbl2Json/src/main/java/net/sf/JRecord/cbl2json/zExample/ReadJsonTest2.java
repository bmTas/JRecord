package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParseException;

import net.sf.cobolToJson.impl.readJson.IProcessFields;
import net.sf.cobolToJson.impl.readJson.JsonToCobol;

public class ReadJsonTest2 {

	public static void main(String[] args) throws JsonParseException, IOException {
		(new JsonToCobol()).readJson("G:/Temp/amsPoDownload_records.json", new ProcessFlds(), null);
		//(new JsonToCobol()).readJson("G:/Temp/DTAR020_tst1.bin.json", new ProcessFlds());
	}


	public static class ProcessFlds implements IProcessFields {

		@Override
		public void endObject(int level, String fieldName) {
			if (level == 1 ) {
				System.out.println();
			}
		}

		@Override
		public void updateField(String fieldName, long value) {
			System.out.print("\t" +fieldName + "=" +value);
			
		}

		@Override
		public void updateField(String fieldName, String value) {
			System.out.print("\t" +fieldName + "=" +value);
		}

		@Override
		public void close() throws IOException {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean isSingleObject() {
			return false;
		}
		
	}
}
