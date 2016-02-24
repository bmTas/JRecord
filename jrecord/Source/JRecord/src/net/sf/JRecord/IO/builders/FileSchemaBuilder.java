package net.sf.JRecord.IO.builders;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;

import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.RecordEditorXmlWriter;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolCopybookIOProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOCopybookProvider;
import net.sf.JRecord.def.IO.builders.Icb2xmlIOProvider;

public class FileSchemaBuilder implements ICobolCopybookIOProvider, IIOCopybookProvider, Icb2xmlIOProvider {
	private static final CopybookLoaderFactory lf = CopybookLoaderFactory.getInstance();
	
	private final int schemaType;

	public FileSchemaBuilder(int schemaType) {
		super();
		this.schemaType = schemaType;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.ICobolIOCopybookProvider#newIOBuilder(java.lang.String)
	 */
	@Override
	public CblIOBuilderMultiSchema newIOBuilder(String copybookFilename) {
    	try {
    		return new CblIOBuilderMultiSchema(copybookFilename, (ICopybookLoaderStream) lf.getLoader(schemaType), ICopybookDialects.FMT_MAINFRAME);
			//return new CblIOBuilderSchemaFilename(copybookFileame, lf.getLoader(schemaType), ICopybookDialects.FMT_MAINFRAME);
		} catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
    }

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.ICobolIOCopybookProvider#newIOBuilder(java.io.InputStream, java.lang.String)
	 */
	@Override
	public CblIOBuilderMultiSchema newIOBuilder(InputStream cobolCopybookStream, String copybookName) {
    	try {
    		return new CblIOBuilderMultiSchema(
    				cobolCopybookStream, copybookName, 
    				(ICopybookLoaderStream) lf.getLoader(schemaType), 
					ICopybookDialects.FMT_MAINFRAME);
//			return new CblIOBuilderSchemaStream( 
//					cobolCopybookStream, copybookName,
//					(ICopybookLoaderStream) lf.getLoader(schemaType), 
//					ICopybookDialects.FMT_MAINFRAME);
		} catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
    }
	
	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICobolCopybookIOProvider#newIOBuilder(java.io.Reader, java.lang.String)
	 */
	@Override
	public ICobolIOBuilder newIOBuilder(Reader copybookReader, String copybookName) {
		try {
			CblIOBuilderMultiSchema ret = new CblIOBuilderMultiSchema(
					 copybookName, 
					(ICopybookLoaderStream) lf.getLoader(schemaType));
			ret.addCopyBook(copybookReader, copybookName);
			return ret;
		} catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @see net.sf.JRecord.IO.builders.ICobolIOCopybookProvider#newMultiCopybookIOBuilder(java.lang.String)
	 */
	@Override 
	public CblIOBuilderMultiSchema newMultiCopybookIOBuilder(String copybookname) {
		try {
			return new CblIOBuilderMultiSchema(copybookname, (ICopybookLoaderStream) lf.getLoader(schemaType));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.IIOCopybookProvider#export(java.lang.String, net.sf.JRecord.External.ExternalRecord)
	 */
	@Override
	public void export(String fileName, ExternalRecord schema) throws Exception {
		export(new BufferedOutputStream(new FileOutputStream(fileName)), schema);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.IIOCopybookProvider#export(java.io.OutputStream, net.sf.JRecord.External.ExternalRecord)
	 */
	@Override
	public void export(OutputStream outStream, ExternalRecord schema) throws Exception {
		RecordEditorXmlWriter writer = new RecordEditorXmlWriter();
		
		writer.writeCopyBook(outStream, schema, new TextLog());
	}
	
	
} 
