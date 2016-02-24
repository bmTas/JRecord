package net.sf.JRecord.IO.builders;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlMultiFileIOBuilder;

public class CblIOBuilderMultiSchema extends CblIOBuilderMultiSchemaBase<CblIOBuilderMultiSchema>
implements ICobolMultiCopybookIOBuilder, Icb2xmlMultiFileIOBuilder {

	/**
	 * Create a IOBuilder for a file based Copybook (schema)
	 * @param copybookFilename copybook file name
	 * @param loader copybook (schema) loader
	 * @param cobolDialect
	 */
	public CblIOBuilderMultiSchema(String copybookFilename, ICopybookLoaderStream loader, int cobolDialect) {
		super(Conversion.getCopyBookId(copybookFilename), loader, cobolDialect);

		copybooks.add(new CreateExternalFromFile(this, copybookFilename));
	}


	/**
	 * Create a IOBuilder for a Stream based Copybook (schema)
	 * 
	 * @param copybookStream copybook stream
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 * @param cobolDialect Cobol Dialect
	 * @throws IOException Any IO Error that occured
	 */
	public CblIOBuilderMultiSchema(InputStream copybookStream, String copybookName, ICopybookLoaderStream loader, int cobolDialect) {
		super(copybookName, loader, cobolDialect);

		copybooks.add(new CreateExternalFromStream(this, copybookStream, copybookName));
	} 

	/**
	 * Create Multicopybook builder
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 */
	public CblIOBuilderMultiSchema(String copybookname, ICopybookLoaderStream loader) {
		super(copybookname, loader);
	}
}
