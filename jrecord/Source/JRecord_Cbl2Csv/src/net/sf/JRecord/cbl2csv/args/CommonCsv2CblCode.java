package net.sf.JRecord.cbl2csv.args;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;

public class CommonCsv2CblCode {

	   /**
     * This method updates field names, converting cobol '-' to _ and (,) to _
     *  
     * @param rec Schema to be updated
     */
    public static void updateCsvNames(LayoutDetail schema, ParseArgsCobol2Csv csvArgs, IDefineCsvFields defineFields) {
 	
		FieldDetail field;
		int fieldCount = schema.getRecord(0).getFieldCount();
		for (int i = 0; i < fieldCount; i++) {
			field = schema.getField(0, i);
			if (TypeManager.isNumeric(field.getType())) {
				defineFields.addCsvField(csvArgs.updateName(field.getName()), Type.ftNumAnyDecimal, 0);
			} else {
				defineFields.addCsvField(csvArgs.updateName(field.getName()), Type.ftChar, 0);
			}
 		}  
    }

}
