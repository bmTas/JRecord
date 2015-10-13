/*
 * @Author Bruce Martin
 * Created on 28/01/2007
 *
 * Purpose:
 * Convert an ExternalRecord into a LayoutDetail (internal form)
 */
package net.sf.JRecord.External;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LayoutGetFieldByName;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.Def.ExternalField;


/**
 * Convert an ExternalRecord (interface format) into a LayoutDetail (internal format)
 *
 * @author Bruce Martin
 *
 */
public class ToLayoutDetail {

    private static ToLayoutDetail instance = null;

	/**
	 * onvert an ExternalRecord into a LayoutDetail (internal form)
	 *
	 * @param recordDefinition Standard record definition
	 *
	 * @return Group of records
	 */
	public LayoutDetail getLayout(ExternalRecord recordDefinition) {

		if (recordDefinition == null) {
			return null;
		}
	    LayoutDetail ret = null;

	    RecordDetail[] records;
	    String recordSepString = recordDefinition.getRecSepList();

	    String fontName = recordDefinition.getFontName();
	    byte[] recordSep = CommonBits.getEolBytes( recordDefinition.getRecordSep(), recordSepString, fontName);

		
		if (recordDefinition.getNumberOfRecords() == 0) {
	        records = new RecordDetail[1];
	        records[0] = toRecordDetail(recordDefinition);
	        records[0].updateRecordSelection(recordDefinition.getRecordSelection(), records[0]);
//		    ExternalSelection recordSelection = recordDefinition.getRecordSelection();
//		    if (recordSelection != null && recordSelection.getSize() > 0) {
//		    	layouts[0].getRecordSelection().setRecSel((new Convert()).convert(recordSelection, layouts[0]));
//		    }
	        ret = genSchema(recordDefinition, records, recordSepString, recordSep);
	    } else {
	        records = new RecordDetail[recordDefinition.getNumberOfRecords()];
	        for (int i = 0; i < records.length; i++) {
	            records[i] = toRecordDetail(recordDefinition.getRecord(i));
	        }    

	        ret = genSchema(recordDefinition, records, recordSepString, recordSep);
		    for (int i = 0; i < records.length; i++) {
		    	records[i].updateRecordSelection(
		    			recordDefinition.getRecord(i).getRecordSelection(), 
		    			new LayoutGetFieldByName(ret,  records[i]));
//			    ExternalSelection recordSelection = recordDefinition.getRecord(i).getRecordSelection();
//			    if (recordSelection != null && recordSelection.getSize() > 0) {
//			    	layouts[i].getRecordSelection().setRecSel(
//			    			(new Convert()).convert(recordSelection, new GetField(ret,  layouts[i])));
//			    }
		    }
	    }
	
	    ret.setDelimiter(recordDefinition.getDelimiter());
	    //ret.setLineNumberOfFieldNames(recordDefinition.getLineNumberOfFieldNames());

	    return ret;
	}


	/**
	 * @param recordDefinition
	 * @param layouts
	 * @param recordSepString
	 * @param recordSep
	 * @return
	 */
	private LayoutDetail genSchema(ExternalRecord recordDefinition,
			RecordDetail[] layouts, String recordSepString, byte[] recordSep) {
		return new LayoutDetail(recordDefinition.getRecordName(),
	            layouts,
	            recordDefinition.getDescription(),
	            recordDefinition.getRecordType(),
	            recordSep,
	            recordSepString,
	            recordDefinition.getFontName(),
	            null,
	            recordDefinition.getFileStructure());
	}


	/**
	 * converts an ExtendedRecord (ie used for storage of records externally)
	 * to the format used in the record editor
	 *
	 * @param def record definition
	 *
	 * @return the same definition as used in the record editor
	 */
	private RecordDetail toRecordDetail(ExternalRecord def) {
	    FieldDetail[] fields = new FieldDetail[def.getNumberOfRecordFields()];
	    ExternalField fieldRec;
	    int i;

	    for (i = 0; i < fields.length; i++) {
	        fieldRec = def.getRecordField(i);
	        fields[i] = new FieldDetail(fieldRec.getName(),
	                fieldRec.getDescription(), fieldRec.getType(),
	                fieldRec.getDecimal(), def.getFontName(), 0, fieldRec.getParameter());

	        if (fieldRec.getLen() < 0) {
	        	fields[i].setPosOnly(fieldRec.getPos());
	        } else {
	        	fields[i].setPosLen(fieldRec.getPos(), fieldRec.getLen());
	        }

	        fields[i].setGroupName(fieldRec.getGroup());
	        fields[i].setDependingOnDtls(fieldRec.getDependOnDtls());

		    String s = fieldRec.getDefault();
		    if (s != null && ! "".equals(s)) {
		    	fields[i].setDefaultValue(s);
		    }
	    }


	    RecordDetail ret = new RecordDetail(def.getRecordName(),
	            def.getRecordPositionOption(),
//	    		def.getTstField(), def.getTstFieldValue(),
	            def.getRecordType(), def.getDelimiter(), def.getQuote(),
	            def.getFontName(), fields, def.getRecordStyle());
	    ret.setParentRecordIndex(def.getParentRecord());
	    ret.setDependingOn(def.getDependingOn());

//	    if (def.getRecordSelection() != null && def.getRecordSelection().getSize() > 0) {
//	    	ret.getRecordSelection().setRecSel((new Convert()).convert(def.getRecordSelection(), ret));
//	    }

	    if (def.isDefaultRecord()) {
	    	ret.getRecordSelection().setDefaultRecord(true);
	    }

	    return ret;
	}


    /**
     * @return Returns the instance.
     */
    public static ToLayoutDetail getInstance() {

        if (instance == null) {
            instance = new ToLayoutDetail();
        }
        return instance;
    }
}
