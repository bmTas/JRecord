package net.sf.JRecord.cbl2json.zTest;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

public class TstReadJSon {

	public static void main(String[] args) throws JsonParseException, FileNotFoundException, IOException {
		JsonFactory jfactory = new JsonFactory();

		/*** read from file ***/
		JsonParser parser = jfactory.createParser(new FileInputStream("G:\\Temp\\DTAR020_B.json"));
		JsonToken lastType, type;
		String indent = "   ";
		
		while( (type = parser.nextToken()) != null) {
			System.out.println(indent + type.name() + " " + parser.getText() + " " + parser.getCurrentName());
			switch (type) {
			case START_ARRAY:
			case START_OBJECT:
				indent += "   ";
				break;
			case END_ARRAY:
			case END_OBJECT:
				indent = indent.substring(3);
			case FIELD_NAME:
				break;
			case VALUE_EMBEDDED_OBJECT:
				break;
			case VALUE_NULL:
				break;
			case VALUE_NUMBER_FLOAT:
				break;
			case VALUE_NUMBER_INT:
				break;
			case VALUE_STRING:
				break;
			case VALUE_FALSE:
				break;
			case VALUE_TRUE:
				break;
			case NOT_AVAILABLE:
				break;
			default:
				break;
			}
		}

	}

}
