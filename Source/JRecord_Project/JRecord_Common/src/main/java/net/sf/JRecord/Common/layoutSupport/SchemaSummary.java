package net.sf.JRecord.Common.layoutSupport;

import java.util.ArrayList;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.cgen.def.ILayoutDetails4gen;
import net.sf.JRecord.cgen.def.IRecordDetail4gen;

public class SchemaSummary implements ISchemaSummary {
	
	private final ArrayList<RecordSummary> records;
	
	private final int fileStructure;
	private final String fontName;
	
	public SchemaSummary(ILayoutDetails4gen schema) {
		int recordCount = schema.getRecordCount();
		this.fileStructure = schema.getFileStructure();
		this.fontName = schema.getFontName();
		
		this.records = new ArrayList<>(recordCount);
		for (int index = 0; index < recordCount; index++) {
			records.add(new RecordSummary(schema.getRecord(index)));
		}
	}
	
	/**
	 * @return the fileStructure The various file structure's are constants in
	 * interface IFileStructureConstants. i.e. IFileStructureConstants.IO_STANDARD_TEXT_FILE
	 */
	@Override
	public int getFileStructure() {
		return fileStructure;
	}

	/**
	 * @return the fontName
	 */
	@Override
	public String getFontName() {
		return fontName;
	}

	/**
	 * Get record count
	 * @return record count
	 */
	@Override
	public int getRecordCount() {
		return records.size();
	}

	/**
	 * Get a specified record
	 * @param recordIndex Index of required record
	 * @return requested record
	 */
	@Override
	public ISchemaRecord getRecord(int recordIndex) {
		return records.get(recordIndex);
	}



	private static class RecordSummary implements ISchemaRecord {
		private final ArrayList<IFieldDetail> fields;
		private final int recordLength;
		private final String name;
		
		
		RecordSummary(IRecordDetail4gen rec) {
			int fieldCount = rec.getFieldCount();
			this.recordLength = rec.getLength();
			this.fields = new ArrayList<>(fieldCount);
			this.name = rec.getRecordName();
			System.out.println("\n" + rec.getRecordName());
			for (int index = 0; index < fieldCount; index++) {
				System.out.println("\t" + rec.getField(index).getName() + "\t" + rec.getField(index).getType());
				fields.add(rec.getField(index));
			}
		}


		/**
		 * @return the fields
		 */
		@Override
		public ArrayList<IFieldDetail> getFields() {
			return fields;
		}


		/**
		 * @return the recordLength
		 */
		@Override
		public int getRecordLength() {
			return recordLength;
		}


		@Override
		public String getRecordName() {
			return name;
		}
	}
}
