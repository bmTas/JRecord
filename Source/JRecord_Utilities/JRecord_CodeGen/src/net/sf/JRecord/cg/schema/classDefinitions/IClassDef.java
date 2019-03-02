package net.sf.JRecord.cg.schema.classDefinitions;

/**
 * Define a type for
 * @author bruce
 *
 */
public interface IClassDef {

	String[] getConversionImport();

	String getClassName();

	String getJrecAs();

	String getDataImport();

	String getCode();

	String generateCall(int type, String variable);

	String generateFromPojo(String variable);

	String generateToPojo(String variable);

}