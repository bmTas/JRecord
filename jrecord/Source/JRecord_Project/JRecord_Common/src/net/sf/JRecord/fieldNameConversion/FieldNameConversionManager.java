package net.sf.JRecord.fieldNameConversion;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.AbstractManager;


/**
 * This class manages the Cobol_Fieldnames to Java_Field name conversion classes
 * (they implement INameConversion). Each of these classes will convert a Cobol_Field_Name
 * to equivalent Java names. Each class implements a different standard method for doing this
 * 
 * 
 * @author Bruce Martin
 *
 */
public class FieldNameConversionManager implements AbstractManager {

	public static final StdCblFieldNameConversion STD_CBL_FIELD_NAME_CONVERSION = new StdCblFieldNameConversion();
	private static final FieldNameConversionManager instance = new FieldNameConversionManager();
	private static IFieldNameConversion currentConversion = STD_CBL_FIELD_NAME_CONVERSION;
	
	private List<IFieldNameConversion> conversions = new ArrayList<IFieldNameConversion>(5);
	
	
	
	private FieldNameConversionManager() {
		register(STD_CBL_FIELD_NAME_CONVERSION);
		register(NoChangeConversion.ASIS);
		register(NoChangeConversion.DOUBLE_UNDERSCORE);
	}
	
	public static FieldNameConversionManager getInstance() {
		return instance;
	}

	public static IFieldNameConversion getCurrentConversion() {
		return currentConversion;
	}

	public static void setCurrentConversion(IFieldNameConversion currentConversion) {
		FieldNameConversionManager.currentConversion = currentConversion;
	}
	public static void setCurrentConversion(int index) {
		FieldNameConversionManager.currentConversion = instance.conversions.get(index);
	}

	
	
	public final void register(IFieldNameConversion conversion) {
		conversions.add(conversion);
	}

	
	@Override
	public int getNumberOfEntries() {
		return conversions.size();
	}

	@Override
	public String getManagerName() {
		return "Cobol_To_Java_Field_Name_Conversion";
	}

	@Override
	public int getKey(int idx) {
		return idx;
	}

	@Override
	public String getName(int idx) {
		return conversions.get(idx).getConversionName();
	}

	
	
}
