/*
 * @Author Bruce Martin
 * Created on 19/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ToLayoutDetail;
import net.sf.JRecord.IO.builders.CblIOBuilderSchemaFilename;
import net.sf.JRecord.IO.builders.CblIOBuilderSchemaStream;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 * This Class Creates a line-reader or line-writer for a Cobol file (Cobol Copybook).
 *
 * <pre>
 * <b>Usage:</b>
 *   
 *        CobolIoProvider ioProvider = CobolIoProvider.getInstance();
 *
 *        try {
 *             AbstractLineReader reader  = ioProvider.getLineReader(
 *                 Constants.IO_TEXT_LINE, ICopybookDialects.FMT_INTEL,
 *                 CopybookLoader.SPLIT_NONE, copybookName, vendorFile
 *             );
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public class CobolIoProvider {

    private static CobolIoProvider instance = new CobolIoProvider();
    private CopybookLoader copybookInt = new CobolCopybookLoader();

    /**
     * Create a new Mainframe-Cobol IOBulder for a file.
     * @param copybookFileame name of the Copybook (or schema file).
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(String copybookFileame) {
    	return newIOBuilder(copybookFileame, ICopybookDialects.FMT_MAINFRAME);
    }
    
    /**
     * Create a new Cobol IOBulder for a file.
     * 
     * @param copybookFileame name of the Copybook (or schema file).
     * @param cobolDialect Cobol Dialect. Values include:<ul>
     *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe cobol
     *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - Open cobol (or GNU Cobol as it is now known).
     *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Old Free Fujitsu Cobol 3. 
     * </ul>
     * 
     * These are the default values (which can be overriden with the appropriate set* method
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(String copybookFileame, int cobolDialect) {
    	return new CblIOBuilderSchemaFilename(copybookFileame, new CobolCopybookLoader(), cobolDialect);
    }

    /**
     * Create a new Mainframe-Cobol IOBulder for a file.
     * @param copybookFileame name of the Copybook (or schema file).
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(InputStream cobolCopybookStream, String copybookName) {
    	return newIOBuilder(cobolCopybookStream, copybookName, ICopybookDialects.FMT_MAINFRAME);
    }
    
    /**
     * Create a new Cobol IOBulder for a file.
     * @param copybookFileame name of the Copybook (or schema file).
     * @param cobolDialect Cobol Dialect. Values include:<ul>
     *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe cobol
     *   <li><b>ICopybookDialects.FMT_OPEN_COBOL</b> - Open cobol (or GNU Cobol as it is now known).
     *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Old Free Fujitsu Cobol 3. 
     * </ul>
     * 
     * These are the default values (which can be overriden with the appropriate set* method
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(InputStream cobolCopybookStream, String copybookName, int cobolDialect) {
    	return new CblIOBuilderSchemaStream(cobolCopybookStream, copybookName, new CobolCopybookLoader(), cobolDialect);
    }
    

    /**
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (options include mainframe, Fujitu, PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption, int copybookFormat,
			   String copybookName, String filename)
    throws Exception {
        return getLineReader(fileStructure,
 			   numericType, splitOption, copybookFormat,
			   copybookName, filename,
			   null);
    }

    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption,
			   String copybookName, String filename)
					   throws Exception {
     return getLineReader(fileStructure,
			   numericType, splitOption,
			   copybookName, filename,
			   null);
 }

    /**
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (is mainframe, Fujitu PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @param provider line provider (to build your own lines)
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
 			   int numericType, int splitOption,
 			   String copybookName, String filename,
 			   LineProvider provider)
     throws Exception {
    	return getLineReader(fileStructure, numericType, splitOption, CommonBits.getDefaultCobolTextFormat(), copybookName, filename, provider);
    }
    
    /**
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (is mainframe, Fujitu PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookFormat format of the copybook e.g. Cb2xmlConstants.USE_*
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @param provider line provider (to build your own lines)
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption, int copybookFormat,
			   String copybookName, String filename,
			   LineProvider provider)
    throws Exception {
        AbstractLineReader ret;
        String font = "";
        if (numericType == ICopybookDialects.FMT_MAINFRAME) {
            font = "cp037";
        }
       	LayoutDetail copyBook = ToLayoutDetail.getInstance().getLayout(
       	     copybookInt.loadCopyBook(
                        copybookName,
                        splitOption, 0, font,
                        copybookFormat,
                        numericType, 0, null
                ).setFileStructure(fileStructure)
        );

//       	if (provider == null) {
//       		provider = LineIOProvider.getInstance().getLineProvider(fileStructure, font);
//       	}
       	ret = LineIOProvider.getInstance()
       				.getLineReader(copyBook, provider);
       	ret.open(filename, copyBook);

       	return ret;

    }


    /**
     * Create a line writer for a Cobol File
     * 
     * @param fileStructure structure of the output file
     * @return Line writer for the file
     */ @Deprecated
    public AbstractLineWriter getLineWriter(int fileStructure, String outputFileName)
    throws IOException {
        AbstractLineWriter ret = LineIOProvider.getInstance()
        			.getLineWriter(fileStructure);
        ret.open(outputFileName);
        return ret;
    }

 

     /**
      * Create a line writer for a Cobol File
      * 
      * @param fileStructure structure of the output file
      * @return Line writer for the file
      */
     public AbstractLineWriter getLineWriter(IBasicFileSchema schema, String outputFileName)
     throws IOException {
         AbstractLineWriter ret = LineIOProvider.getInstance()
         			.getLineWriter(schema);
         ret.open(outputFileName);
         return ret;
     }


    /**
     * Get a CobolIoProvider
     * 
     * @return Returns the instance.
     */
    public static CobolIoProvider getInstance() {
        return instance;
    }
}
