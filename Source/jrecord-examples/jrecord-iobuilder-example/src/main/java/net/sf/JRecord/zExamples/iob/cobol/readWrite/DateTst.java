package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.LocalDate;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cgen.def.IFieldSerDes;
import net.sf.JRecord.cgen.support.DateFieldSerDes;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class DateTst {

	public static void main(String[] args) throws FileNotFoundException, IOException {
		RecordDatetst rDatetst = new RecordDatetst();
		IFieldSerDes<LocalDate> 
				cyymmdd  = DateFieldSerDes.CYYMMDD,
				yymmdd   = DateFieldSerDes.YYMMDD,
				yyyymmdd = DateFieldSerDes.YYYYMMDD,
				ddmmyy   = DateFieldSerDes.getDDMMYY(),
				dd_mth_yyyy = DateFieldSerDes.getDD_Mth_YYYY(' '),
				yyyy_mm_dd  = DateFieldSerDes.getYYYY_MM_DD('-'),
				dd_mm_yyyy  = DateFieldSerDes.getDD_MM_YYYY('-'),
				dd_mm_yyyy_Dot  = DateFieldSerDes.getDD_MM_YYYY('.');
		
		
		String copybookName = DateTst.class.getResource("DateTst.cbl").getFile();
        ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
        		.newIOBuilder(copybookName)
        			.setDialect( ICopybookDialects.FMT_MAINFRAME)
        			.setFont(Conversion.DEFAULT_ASCII_CHARSET)
        			.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
        			;
        
        AbstractLineWriter writer = ioBldr.newWriter("/home/bruce/work/temp/DateTst.bin");

        LocalDate date = LocalDate.of(1998, 6, 9);
        
        for (int i = 0; i < 30; i++) {
            AbstractLine line = ioBldr.newLine();

            dd_mth_yyyy.setField(line.getFieldValue(rDatetst.ddMthYyyy), date);
            yyyy_mm_dd.setField(line.getFieldValue(rDatetst.yyyyMmDd), date);
            dd_mm_yyyy.setField(line.getFieldValue(rDatetst.ddMmYyyy), date);
            dd_mm_yyyy_Dot.setField(line.getFieldValue(rDatetst.ddMmYyyyDot), date);
            cyymmdd.setField(line.getFieldValue(rDatetst.cyymmdd), date);
            yymmdd.setField(line.getFieldValue(rDatetst.yymmdd), date);
            ddmmyy.setField(line.getFieldValue(rDatetst.ddmmyy), date);
            yyyymmdd.setField(line.getFieldValue(rDatetst.yyyymmdd), date);

            date= date.plusDays(110);
            writer.write(line);
        }
        
        writer.close();
	}

	
	    public static class RecordDatetst {
	       public final String ddMthYyyy = "dd-mth-yyyy";
	       public final String yyyyMmDd = "yyyy-mm-dd";
	       public final String ddMmYyyy = "dd-mm-yyyy";
	       public final String ddMmYyyyDot = "dd-mm-yyyy-dot";
	       public final String cyymmdd = "cyymmdd";
	       public final String yymmdd = "yymmdd";
	       public final String ddmmyy = "ddmmyy";
	       public final String yyyymmdd = "yyyymmdd";
		
	    }


}
