package net.sf.cobolToJson;

import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.def.Icb2xml2Json;
import net.sf.cobolToJson.impl.Cobol2JsonImp;
import net.sf.cobolToJson.impl.ConvertOptions;
import net.sf.cobolToJson.impl.RecordParent;
import net.sf.cobolToJson.impl.RecordSelect;

/**
 * This class creates the Cobol <==> Json and cb2xml <==> Json builders
 * 
 * @author Bruce Martin
 *
 */
public class Cobol2Json  {
	public static final JRecordConstantVars JR_CONSTANTS = JRecordConstantVars.INSTANCE;
	public static final ArrayElementChecks  ARRAY_CHECK_BUILDER = ArrayElementChecks.INSTANCE;
	

	public static ICobol2Json newCobol2Json(String cobolCopybook) {
		return Cobol2JsonImp.newCobol2Json(cobolCopybook);
	}
	
	
	public static ICobol2Json newCobol2Json(InputStream cobolCopybook, String copybookName)  {
		return Cobol2JsonImp.newCobol2Json(cobolCopybook, copybookName);
	}

	public static ICobol2Json newCobol2Json(Reader cobolCopybookReader, String copybookName) {
		return Cobol2JsonImp.newCobol2Json(cobolCopybookReader, copybookName);
	}

	
	public static Icb2xml2Json newCb2Xml2Json(String cobolCopybook) {
		return Cobol2JsonImp.newCb2Xml2Json(cobolCopybook);
	}
	
	
	public static Icb2xml2Json newCb2Xml2Json(InputStream cobolCopybook, String copybookName) {
		return Cobol2JsonImp.newCb2Xml2Json(cobolCopybook, copybookName);
	}


	protected static Icb2xml2Json newJsonConverter(ConvertOptions opts) {
		Icb2xml2Json cbl2json;
		if (opts.useCobol) {
			cbl2json = newCobol2Json(opts.cobolCopybook)
								.setDialect(opts.dialect);
		} else {
			cbl2json = newCb2Xml2Json(opts.cb2xmlCopybook);
		}
		setOpts(cbl2json, opts);
	
		return cbl2json;
	}



	private static void setOpts(Icb2xml2Json cbl2json, ConvertOptions opts) {

		cbl2json
				.setFont(opts.font)
				.setFileOrganization(opts.fileOrganisation)
				.setDropCopybookNameFromFields(opts.dropCopybookName)
				.setTagFormat(opts.tagFormat)
				.setSplitCopybook(opts.split);

		for (RecordSelect rs : opts.recordSelect) {
			cbl2json.setRecordSelection(rs.recordName, Cobol2Json.newFieldSelection(rs.fieldName, rs.value));
		}

		for (RecordParent rp : opts.recordParents) {
			cbl2json.setRecordParent(rp.recordName, rp.parentName);
		}
	}
	
	
	/**
	 * Create a Record-Selection based on field / value test
	 * @param fieldName name of the field to be checked
	 * @param value value to be tested against
	 * @return requested check
	 */
    public static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
    public static ExternalFieldSelection newFieldSelection(String fieldName, String op, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value, op);
    	r.setCaseSensitive(false);
    	return r;
    }
   
    public static ExternalGroupSelection<ExternalSelection> newAnd(ExternalSelection... selections) {
    	return ExternalGroupSelection.newAnd(selections);
    }
    
    public static ExternalGroupSelection<ExternalSelection> newOr(ExternalSelection... selections) {
    	return ExternalGroupSelection.newOr(selections);
    }
}
