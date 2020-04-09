package net.sf.JRecord.cbl2csv.imp;

import java.io.BufferedWriter;

public class RecordWriterDetails {
	public final BufferedWriter writer;
	public final String recordName;
	
	
	public RecordWriterDetails(String recordName, BufferedWriter writer) {
		super();
		this.writer = writer;
		this.recordName = recordName;
	}
	
	
}
