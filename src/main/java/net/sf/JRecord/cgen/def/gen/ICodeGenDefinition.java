package net.sf.JRecord.cgen.def.gen;

/**
 * This interface supplies details to CodeGen so that it can generate Java code.
 * Initially it is being used for RecordDecider definition
 * .
  * @author Bruce Martin
 *
 */
public interface ICodeGenDefinition {
	
	/**
	 * class to import
	 * @return class name that needs to be imported
	 */
	public String importClass();
	/**
	 * This method will return code to construct a class in CodeGen.
	 * Initially it is being used for RecordDecider definition
	 * 
	 * @return ConstrUctor definition
	 */
	public String createClass();
}
