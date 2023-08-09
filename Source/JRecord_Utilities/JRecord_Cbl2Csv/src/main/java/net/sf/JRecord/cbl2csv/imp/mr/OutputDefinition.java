package net.sf.JRecord.cbl2csv.imp.mr;

import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;

public class OutputDefinition {
	final Cobol2CsvMrAdvancedBldr parent;
	private final AbstractLineReader reader;
	//private ArrayList<OutputFile> outputs = new ArrayList<OutputFile>();
	ArrayList<List<RecordDefinition>> recordOutputs = new ArrayList<List<RecordDefinition>>();
	
	
	public OutputDefinition(Cobol2CsvMrAdvancedBldr parent, AbstractLineReader reader) {
		super();
		this.parent = parent;
		this.reader = reader;
	}

	
	public final LayoutDetail getLayout() {
		return reader.getLayout();
	}


	/**
	 * Define where a <i>Cobol-Record type</i> is to be written and what should be written 
	 * @param outputFilename output Csv file name
	 * @param recordName Cobol record name to be written
	 * @return OutputFile definition so you can define the fields to be written.
	 * @throws IOException any IO errors
	 */
	public RecordDefinition addRecordDefinition(String outputFilename, String recordName) throws IOException {
		return addRecordDefinition(new FileWriter(outputFilename), recordName, "");
	}

	/**
	 * Define where a <i>Cobol-Record type</i> is to be written and what should be written 
	 * @param outputFilename output Csv file name
	 * @param recordName Cobol record name to be written
	 * @param encoding Characterset encoding
	 * @return OutputFile definition so you can define the fields to be written.
	 * @throws IOException any IO errors
	 */
	public RecordDefinition addRecordDefinition(String outputFilename, String recordName, String encoding) throws IOException {
		return addRecordDefinition(
				new OutputStreamWriter(
						new FileOutputStream(outputFilename), 
						encoding), 
				recordName, encoding);
	}

	/**
	 * Define where a <i>Cobol-Record type</i> is to be written and what should be written 
	 * @param writer Where the Csv file is to be written
	 * @param recordName  Cobol record name to be written
	 * @return OutputFile definition so you can define the fields to be written.
	 */
	public RecordDefinition addRecordDefinition(Writer writer, String recordName) {
		return addRecordDefinition(writer, recordName, "");

	}
	
	private RecordDefinition  addRecordDefinition(Writer writer, String recordName, String encoding) {
		RecordDefinition outputFile = new RecordDefinition(this, recordName, writer, encoding, parent.fieldSeparator, parent.quote);
		
		int recordIndex = outputFile.getRecordIndex();
		for (int i = recordOutputs.size(); i <= recordIndex; i++) {
			recordOutputs.add(new ArrayList<RecordDefinition>(1));
		}
		recordOutputs.get(recordIndex).add(outputFile);

		return outputFile;
	}

	/**
	 * Generate the Csv Files.
	 * @return Csv Definition
	 * @throws IOException any IO Exception that occurs
	 */
	public  Cobol2CsvMrAdvancedBldr writeCsvFiles() throws IOException {
		LayoutDetail layout = reader.getLayout();
		
		for (List<RecordDefinition> recList : recordOutputs) {
			for (RecordDefinition f : recList) {
				f.writeHeader();
			}
		}
		
		AbstractLine line;
		AbstractLine[] lines = new AbstractLine[layout.getRecordCount()];
		while ((line = reader.read()) != null) {
			int recordIndex = line.getPreferredLayoutIdx();
			
			lines[recordIndex] = line;
			if(recordIndex < 0) {
				System.err.println("Can not determine recordId: " + line.getFullLine());
			} else if (recordIndex < recordOutputs.size()){
				for (RecordDefinition f : recordOutputs.get(recordIndex)) {
					f.writeRecord(lines);
				}
			}
		}
		HashSet<Writer> closed = new HashSet<>(lines.length * 3);
		for (List<RecordDefinition> recList : recordOutputs) {
			for (RecordDefinition f : recList) {
				Writer writer = f.getWriter();
				if (! closed.contains(writer)) {
					writer.close();
					closed.add(writer);
				}
			}
		}
		
		return parent; 
	}
	
	public Cobol2CsvMrAdvancedBldr endOutputDefinition() {
		return parent; 
	}
}
