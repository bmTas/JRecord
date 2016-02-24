package net.sf.JRecord.IO.builders;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByLength;
import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByPosition;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;

/**
 * IO Builder for Fixed Width files
 * 
 * @author Bruce Martin
 *
 */
public class FixedWidthIOBuilder extends CblIOBuilderBase<IFixed> 
implements IFixed /*, IFixedWidthIOBuilder, IDefineFixedFieldsByPosition, IDefineFixedFieldsByLength*/ {

	private ExternalRecord record = new ExternalRecord();
	private boolean definedField = false;

	
	/**
	 * IO Builder for Fixed Width files
	 */ 
	private FixedWidthIOBuilder() {
		super(0);
		setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
	}

	@Override
	protected ExternalRecord getExternalRecordImpl() {
		record.setFontName(super.getFont());
		return record;
	} 


	/**
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#checkOk(boolean)
	 * 
	 * Used to validate schema prior to creating readers / writers
	 */
	@Override
	protected void checkOk(boolean input) {
		if (! definedField) {
			throw new RuntimeException("You must define Fields before getting a Reader/Writer");
		}
	}

	/**
	 * Define Fixed Width fields by position in the record
	 */
	@Override
	public IDefineFixedFieldsByPosition defineFieldsByPosition() {
		return this;
	}
	
	/**
	 * Define Fixed Width fields by length
	 * 
	 * @return
	 */
	@Override
	public IDefineFixedFieldsByLength defineFieldsByLength() {
		return this;
	}


		 
	public IDefineFixedFieldsByPosition addFieldByPosition(String name, int type, int pos, int decimal) {
		record.addFieldByPosition(name, type, pos, decimal);
		super.clearLayout();
		return this;
	}
	
	/**
	 * Skip a field starting a a specified position
	 * 
	 * @param pos position of field to be skipped 
	 * 
	 * @return This schema builder so more fields can be added.
	 */
	@Override
	public IDefineFixedFieldsByPosition skipFieldPosition(int pos) {
		record.skipFieldPosition(pos);
		super.clearLayout();
		return this;
	}

	@Override
	public IDefineFixedFieldsByLength addFieldByLength(String name, int type, int length, int decimal) {
		record.addFieldByLength(name, type, length, decimal);
		super.clearLayout();

		return this;
	}
	
	/**
	 * Skip a specified number of bytes in the record.
	 * @param numberOfBytes number of bytes to be skipped
	 * @return
	 */
	@Override
	public IDefineFixedFieldsByLength skipBytes(int numberOfBytes) {
		record.skipBytes(numberOfBytes);
		super.clearLayout();
		return this;			
	}

	/**
	 * @see net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByLength#endOfRecord()
	 */
	@Override
	public FixedWidthIOBuilder endOfRecord() {
		definedField = true;
		return this;
	}


	/**
	 *  @see net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByPosition#endOfRecord(int)
	 */
	@Override
	public FixedWidthIOBuilder endOfRecord(int position) {
		record.skipFieldPosition(position);
		definedField = true;
		return this;
	}
		
		
	
	
	public static IFixedWidthIOBuilder newFixedWidthIOBuilder() {
		return new FixedWidthIOBuilder();
	}
}
