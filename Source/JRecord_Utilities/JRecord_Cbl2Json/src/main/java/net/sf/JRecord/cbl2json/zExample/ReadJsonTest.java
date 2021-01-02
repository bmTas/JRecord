package net.sf.JRecord.cbl2json.zExample;

import java.io.File;
import java.io.IOException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

public class ReadJsonTest {

	public static void main(String[] args) throws JsonParseException, IOException {
		(new ReadJsonTest()).readJson("G:/Temp/amsPoDownload_records.json");
		//(new ReadJsonTest()).readJson("G:/Temp/DTAR020_tst1.bin.json");
	}

	JsonFactory factory = new JsonFactory();
	private void readJson(String jsonFileName) throws IOException, JsonParseException {
		//JsonParser  parser  = factory.createParser(new File(jsonFileName));
		ParserMgr parserMgr = new ParserMgr(factory.createParser(new File(jsonFileName)));
		ArrayMgr arrayMgr = new ArrayMgr(parserMgr);
		int objIdx = 0;
	    //JsonToken jsonToken = parser.nextToken();
		
		if (parserMgr.moreData()){
			parserMgr.nextToken();
			while(parserMgr.moreData()){
				
				switch (parserMgr.jsonToken)  {
				case START_OBJECT:	objIdx++;					break;
				case END_OBJECT:
					if (--objIdx ==1 ) {
						System.out.println();
					} else if (objIdx < 0) {
						objIdx = 0;
					}
					break;
				case START_ARRAY:	arrayMgr.startArray();		break;
				case END_ARRAY:		arrayMgr.endArray();		break;
				case FIELD_NAME:	parserMgr.setFieldName();	break;
				case VALUE_NUMBER_INT:
					System.out.print("\t" +parserMgr.fieldName + "=" +parserMgr.parser.getValueAsLong());
					break;
				case VALUE_NUMBER_FLOAT:
				case VALUE_FALSE:
				case VALUE_TRUE:
				case VALUE_STRING:
					System.out.print("\t" +parserMgr.fieldName + "=" +parserMgr.parser.getValueAsString());
					break;
//				case VALUE_NUMBER_FLOAT:
//					System.out.print("\t" +parserMgr.fieldName + "=" +parserMgr.parser.getValueAsString());
//					break;
				}

			parserMgr.nextToken();
			}
		}
	}

	private static class ParserMgr {
		final JsonParser  parser;
		JsonToken jsonToken;
		String fieldName="";
		
		public ParserMgr(JsonParser parser) {
			super();
			this.parser = parser;
		}

		public JsonToken nextToken() throws IOException {
			return (jsonToken = parser.nextToken());
		}
	
		boolean moreData() {
			return !parser.isClosed();
		}
		
		void setFieldName() throws IOException {
			fieldName = parser.getText();
			if (fieldName == null) {
				fieldName = "";
			}
		}
	}
	
	private static class ArrayMgr {
		final ParserMgr parserMgr;
		int idxNum = 0;
		int[] indexs = new int[20];
		
		public ArrayMgr(ParserMgr parserMgr) {
			super();
			this.parserMgr = parserMgr;
		}
	
		void startArray() {
			indexs[idxNum++] = 0;
		}
		void endArray() {
			idxNum--;
		}
		
	}

}
