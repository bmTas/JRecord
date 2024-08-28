package codeGen.readWrite.io;

 import java.io.IOException;

/*
  * *------------- Keep this notice in your final code ---------------
  * *   This code was generated by JRecord projects CodeGen program
  * *            on the: 26 Nov 2017 9:46:29 
  * *     from Copybook: DTAR022.cbl
  * *          Template: pojo
  * *             Split: None   
  * * File Organization: 0   
  * *              Font: cp037
  * *   
  * *    CodeGen Author: Bruce Martin
  * *-----------------------------------------------------------------
  *
  *   This Code should not be changed you should, either:
  *   * Rerun CodeGen to regenerate it 
  *   * Fix CodeGen and rerun CodeGen
  *
  *   Please supply any program fixes/enhancements/documentation
  *   back to the JRecord project (https://sourceforge.net/projects/jrecord/)
  *   so other people can benefit !!!
  * 
  *
  *          Bruce Martin (JRecord / CodeGen Author) 
  *
  * ------------------------------------------------------------------
  * v01  CodeGen        26 Nov 2017  Initial version
  *     (Bruce Martin)
  */

import java.io.InputStream;

import codeGen.readWrite.data.ConvertDtar022;
import codeGen.readWrite.data.LineDtar022Pojo;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cgen.impl.io.IoBuilder;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class IoBuilderDtar022  {

   public static IoBuilder<LineDtar022Pojo> newIoBuilder(String copybookName) {

        return createIoBuilder(updateIoBuilder(
                                       JRecordInterface1.COBOL
                                            .newIOBuilder(copybookName)));
    }
    
   public static IoBuilder<LineDtar022Pojo> newIoBuilder(InputStream copybookStream, String copybookName) {

        return createIoBuilder(updateIoBuilder(
                                       JRecordInterface1.COBOL
                                            .newIOBuilder(copybookStream, copybookName)));
   }
    
   private static IoBuilder<LineDtar022Pojo> createIoBuilder(ICobolIOBuilder iob) {
    
       try {
            /** IoBuilder requires JRecord Version 0.81.5 or later */
           return new IoBuilder<LineDtar022Pojo>(new ConvertDtar022(iob), iob);
       } catch (IOException e) {
           throw new RuntimeException(e);
       }
    }


   private static ICobolIOBuilder updateIoBuilder(ICobolIOBuilder iob) {
    
        return iob
                                   .setFont("cp037")
                                   .setFileOrganization(IFileStructureConstants.IO_DEFAULT)
                                   .setSplitCopybook(CopybookLoader.SPLIT_NONE)
        ;
   }
    
}