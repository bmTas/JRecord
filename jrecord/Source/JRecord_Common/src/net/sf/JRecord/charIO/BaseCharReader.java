package net.sf.JRecord.charIO;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public abstract class BaseCharReader implements ICharReader {

	protected BufferedReader reader;


	@Override
	public void open(String fileName, String font) throws IOException {
	    open(new FileInputStream(fileName), font);
	}

	@Override
	public void open(InputStream inputStream, String font) throws IOException {
		InputStreamReader stdReader;
		if (font == null || font.length() == 0) {
		    stdReader = new InputStreamReader(inputStream);
		} else {
		    try {
		        stdReader = new InputStreamReader(inputStream, font);
		    } catch (Exception e) {
		        stdReader = new InputStreamReader(inputStream);
		    }
		}
	
		reader = new BufferedReader(stdReader);
	
	}

	@Override
	public void close() throws IOException {
		reader.close();
	}

}