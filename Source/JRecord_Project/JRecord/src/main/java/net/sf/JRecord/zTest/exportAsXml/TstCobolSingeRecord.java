package net.sf.JRecord.zTest.exportAsXml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeManager.CharsetType;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.IFileIOBuilder;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Testing
 * 
 * * Writing a Schema as Xml and then reading it back in.
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolSingeRecord extends TestCase {

	private static final String OCCURS_DEPENDING = "OccursDepending";
	private static final String CONDITION = "<condition";
	private static final String DTAR107 = "DTAR107";

	private static final String BIT_OF_EVERYTHING_01 = "BitOfEverything01";
	private static final String BIT_OF_EVERYTHING_02 = "BitOfEverything02";

	private static final String END_CONDITION = "</condition>";

	private String[] basicCopybooks = {
			"Cbl_Line_Test_Record", "DTAR020",  DTAR107,
			"FCUSDAT",              "RBIVCopy", "RightJust",
			BIT_OF_EVERYTHING_02,
			BIT_OF_EVERYTHING_01,
			OCCURS_DEPENDING
	};

	private FieldDetail[][] expectFieldsForCopybooks = {
	{
		createField("Fld-Char", ".Cbl-Line-Test-Record.",1, 10, 0, 0),
		createField("Fld-Char-right-just", ".Cbl-Line-Test-Record.",11, 10, 0, 0),
		createField("Fld-Decimal", ".Cbl-Line-Test-Record.",21, 5, 0, 31),
		createField("Filler-1", ".Cbl-Line-Test-Record.",26, 8, 0, 0),
		createField("Filler-2", ".Cbl-Line-Test-Record.",34, 4, 0, 0),
		createField("Filler-3", ".Cbl-Line-Test-Record.",38, 8, 0, 0),
		createField("Fld-Num-Right-Just", ".Cbl-Line-Test-Record.",46, 8, 0, 6),
		createField("Fld-Num-Right-Just1", ".Cbl-Line-Test-Record.",54, 8, 0, 25),
		createField("Fld-Postive-Int", ".Cbl-Line-Test-Record.",62, 4, 0, 35),
		createField("Fld-Assummed-Decimal", ".Cbl-Line-Test-Record.",66, 10, 4, 32),
		createField("Fld-Num-2-decimal", ".Cbl-Line-Test-Record.",76, 10, 2, 6),
		createField("Fld-Decimal-2-digits", ".Cbl-Line-Test-Record.",86, 5, 2, 31),
		createField("Fld-Positive-Int-2-digit", ".Cbl-Line-Test-Record.",91, 4, 2, Type.ftPositiveBinaryBigEndian),
		createField("Filler-4", ".Cbl-Line-Test-Record.",95, 6, 0, 0),
		createField("Fld-Mainframe-Int", ".Cbl-Line-Test-Record.",101, 4, 0, 35),
		createField("Fld-MainframeInt2decimal", ".Cbl-Line-Test-Record.",105, 4, 2, 35),
		createField("Filler-5", ".Cbl-Line-Test-Record.",109, 1, 0, 0),
		createField("Filler-6", ".Cbl-Line-Test-Record.",110, 2, 0, 0),
		createField("Filler-7", ".Cbl-Line-Test-Record.",112, 4, 0, 0),
		createField("Filler-8", ".Cbl-Line-Test-Record.",116, 8, 0, 0),
		createField("Fld-Mainframe-Small-Int", ".Cbl-Line-Test-Record.",124, 2, 0, 35),
		createField("Fld-Mainframe-Long", ".Cbl-Line-Test-Record.",126, 8, 0, 35),
		createField("Mainframe-Packed-Decimal", ".Cbl-Line-Test-Record.",134, 3, 0, 31),
		createField("Mainframe-Packed-DecimalP", ".Cbl-Line-Test-Record.",137, 3, 1, 31),
		createField("Fld-Zoned", ".Cbl-Line-Test-Record.",140, 2, 0, 32),
		createField("Fld-Zoned-decimalp", ".Cbl-Line-Test-Record.",142, 4, 2, 32),
		createField("Filler-3", ".Cbl-Line-Test-Record.",146, 5, 0, 0),
	}, {
		createField("DTAR020-KEYCODE-NO", ".DTAR020-KCODE-STORE-KEY.",1, 8, 0, 0),
		createField("DTAR020-STORE-NO", ".DTAR020-KCODE-STORE-KEY.",9, 2, 0, 31),
		createField("DTAR020-DATE", "",11, 4, 0, 31),
		createField("DTAR020-DEPT-NO", "",15, 2, 0, 31),
		createField("DTAR020-QTY-SOLD", "",17, 5, 0, 31),
		createField("DTAR020-SALE-PRICE", "",22, 6, 2, 31),
	}, {
		createField("DTAR107-STORE-NO", "",1, 2, 0, 31),
		createField("DTAR107-STORE-NO-REDEF", "..",1, 2, 0, 0),
		createField("DTAR107-TRANS-DATE", "",3, 4, 0, 31),
		createField("DTAR107-CUST-NO", "",7, 16, 0, 25),
		createField("DTAR107-AMOUNT", "",23, 5, 2, 31),
		createField("DTAR107-OPERATOR-NO", "",28, 5, 0, 31),
		createField("DTAR107-TERMINAL-NO", "",33, 2, 0, 31),
		createField("DTAR107-TIME", "",35, 3, 0, 31),
		createField("DTAR107-TRANS-NO", "",38, 3, 0, 31),
		createField("DTAR107-TRANS-TYPE", "",41, 2, 0, 25),
		createField("DTAR107-TRANS-CODE", "",43, 2, 0, 25),
		createField("DTAR107-STD-POINTS", "",45, 4, 0, 31),
		createField("DTAR107-BONUS-POINTS", "",49, 4, 0, 31),
		createField("DTAR107-NO-OF-TXNS", "",53, 2, 0, 25),
	}, {
		createField("CUSTOMER-ID", ".CUSTOMER-DATA.",1, 6, 0, 25),
		createField("CUSTOMER-NAME", ".CUSTOMER-DATA.PERSONAL-DATA.",7, 20, 0, 0),
		createField("CUSTOMER-ADDRESS", ".CUSTOMER-DATA.PERSONAL-DATA.",27, 20, 0, 0),
		createField("CUSTOMER-PHONE", ".CUSTOMER-DATA.PERSONAL-DATA.",47, 8, 0, 0),
		createField("TRANSACTION-NBR", ".CUSTOMER-DATA.TRANSACTIONS.",55, 4, 0, Type.ftPositiveBinaryBigEndian),
		createField("TRANSACTION-DATE (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",59, 8, 0, 0),
		createField("TRANSACTION-DAY (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",59, 2, 0, 0),
		createField("TRANSACTION-MONTH (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",62, 2, 0, 0),
		createField("TRANSACTION-YEAR (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",65, 2, 0, 0),
		createField("TRANSACTION-AMOUNT (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",67, 8, 2, 31),
		createField("TRANSACTION-COMMENT (0)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",75, 9, 0, 0),
		createField("TRANSACTION-DATE (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",84, 8, 0, 0),
		createField("TRANSACTION-DAY (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",84, 2, 0, 0),
		createField("TRANSACTION-MONTH (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",87, 2, 0, 0),
		createField("TRANSACTION-YEAR (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",90, 2, 0, 0),
		createField("TRANSACTION-AMOUNT (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",92, 8, 2, 31),
		createField("TRANSACTION-COMMENT (1)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",100, 9, 0, 0),
		createField("TRANSACTION-DATE (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",109, 8, 0, 0),
		createField("TRANSACTION-DAY (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",109, 2, 0, 0),
		createField("TRANSACTION-MONTH (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",112, 2, 0, 0),
		createField("TRANSACTION-YEAR (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",115, 2, 0, 0),
		createField("TRANSACTION-AMOUNT (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",117, 8, 2, 31),
		createField("TRANSACTION-COMMENT (2)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",125, 9, 0, 0),
		createField("TRANSACTION-DATE (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",134, 8, 0, 0),
		createField("TRANSACTION-DAY (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",134, 2, 0, 0),
		createField("TRANSACTION-MONTH (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",137, 2, 0, 0),
		createField("TRANSACTION-YEAR (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",140, 2, 0, 0),
		createField("TRANSACTION-AMOUNT (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",142, 8, 2, 31),
		createField("TRANSACTION-COMMENT (3)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",150, 9, 0, 0),
		createField("TRANSACTION-DATE (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",159, 8, 0, 0),
		createField("TRANSACTION-DAY (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",159, 2, 0, 0),
		createField("TRANSACTION-MONTH (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",162, 2, 0, 0),
		createField("TRANSACTION-YEAR (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION..",165, 2, 0, 0),
		createField("TRANSACTION-AMOUNT (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",167, 8, 2, 31),
		createField("TRANSACTION-COMMENT (4)", ".CUSTOMER-DATA.TRANSACTIONS.TRANSACTION.",175, 9, 0, 0),
	}, { //RBIVCopy
//		createField("RBI-TEXT-1", ".RBI-RECORD-T1.",1, 50, 0, 0),
//		createField("RBI-QTY", ".RBI-RECORD-T1.",51, 3, 0, 25),
//		createField("RBI-NUMBER-S96SLS (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",54, 7, 0, 9),
//		createField("RBI-NUMBER-S96DISP (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",61, 6, 0, 32),
//		createField("RBI-NUMBER-S96CMP3 (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",67, 4, 0, 31),
//		createField("RBI-NUMBER-S96CMP (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",71, 4, 0, 35),
//		createField("RBI-TEXTE-2", ".RBI-RECORD-T1.",75, 50, 0, 0),
//		createField("RBI-TEXT-1", ".RBI-RECORD-T2.",1, 50, 0, 0),
//		createField("RBI-QTY", ".RBI-RECORD-T2.",51, 3, 0, 25),
//		createField("RBI-NUMBER-S96SLS (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",54, 7, 0, 9),
//		createField("RBI-NUMBER-S96DISP (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",61, 6, 0, 32),
//		createField("RBI-NUMBER-S96CMP3 (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",67, 4, 0, 31),
//		createField("RBI-NUMBER-S96CMP (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",71, 4, 0, 35),
//		createField("RBI-NUMBER-S96SLS (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",75, 7, 0, 9),
//		createField("RBI-NUMBER-S96DISP (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",82, 6, 0, 32),
//		createField("RBI-NUMBER-S96CMP3 (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",88, 4, 0, 31),
//		createField("RBI-NUMBER-S96CMP (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",92, 4, 0, 35),
//		createField("RBI-TEXTE-2", ".RBI-RECORD-T2.",96, 50, 0, 0),
		createField("RBI-TEXT-1", ".RBI-RECORD-T1.",1, 50, 0, 0),
		createField("RBI-TEXT-1", ".RBI-RECORD-T2.",1, 50, 0, 0),
		createField("RBI-QTY", ".RBI-RECORD-T1.",51, 3, 0, 25),
		createField("RBI-QTY", ".RBI-RECORD-T2.",51, 3, 0, 25),
		createField("RBI-NUMBER-S96SLS (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",54, 7, 0, 9),
		createField("RBI-NUMBER-S96SLS (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",54, 7, 0, 9),
		createField("RBI-NUMBER-S96DISP (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",61, 6, 0, 32),
		createField("RBI-NUMBER-S96DISP (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",61, 6, 0, 32),
		createField("RBI-NUMBER-S96CMP3 (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",67, 4, 0, 31),
		createField("RBI-NUMBER-S96CMP3 (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",67, 4, 0, 31),
		createField("RBI-NUMBER-S96CMP (0)", ".RBI-RECORD-T1.RBI-REPETITIVE-AREA.RBI-REPEAT.",71, 4, 0, 35),
		createField("RBI-NUMBER-S96CMP (0)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",71, 4, 0, 35),
		createField("RBI-TEXTE-2", ".RBI-RECORD-T1.",75, 50, 0, 0),
		createField("RBI-NUMBER-S96SLS (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",75, 7, 0, 9),
		createField("RBI-NUMBER-S96DISP (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",82, 6, 0, 32),
		createField("RBI-NUMBER-S96CMP3 (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",88, 4, 0, 31),
		createField("RBI-NUMBER-S96CMP (1)", ".RBI-RECORD-T2.RBI-REPETITIVE-AREA.RBI-REPEAT.",92, 4, 0, 35),
		createField("RBI-TEXTE-2", ".RBI-RECORD-T2.",96, 50, 0, 0),
	}, {
		createField("Field-Right-Justified", ".Tst-Record.",1, 20, 0, 1),
		createField("Field-Justified", ".Tst-Record.",21, 20, 0, 1),
		createField("Field-1", ".Tst-Record.",41, 20, 0, 0),
	}, {
		createField("NumA", ".CompFields.", 1, 25, 2, 6),
		createField("NumA", ".Fields-2.", 1, 25, 2, 6),
		createField("NumA", ".CompFields-3.", 1, 25, 2, 6),
		createField("NumB", ".CompFields.", 26, 3, 2, 22),
		createField("NumB", ".Fields-2.", 26, 3, 2, 22),
		createField("NumB", ".CompFields-3.", 26, 3, 2, 22),
		createField("NumC", ".CompFields.", 29, 3, 0, 25),
		createField("NumC", ".Fields-2.", 29, 3, 0, 25),
		createField("NumC", ".CompFields-3.", 29, 3, 0, 25),
		createField("text", ".CompFields.", 32, 20, 0, 0),
		createField("text", ".Fields-2.", 32, 20, 0, 0),
		createField("text", ".CompFields-3.", 32, 20, 0, 0),
		createField("NumD", ".CompFields.", 52, 3, 6, 0),
		createField("NumD", ".Fields-2.", 52, 3, 6, 0),
		createField("NumD", ".CompFields-3.", 52, 3, 6, 0),
		createField("NumE", ".CompFields.", 55, 3, -3, 0),
		createField("NumE", ".Fields-2.", 55, 3, -3, 0),
		createField("NumE", ".CompFields-3.", 55, 3, -3, 0),
		createField("float", ".CompFields.", 58, 4, 0, 17),
		createField("float", ".Fields-2.", 58, 4, 0, 17),
		createField("float", ".CompFields-3.", 58, 4, 0, 17),
		createField("double", ".CompFields.", 62, 8, 0, 18),
		createField("double", ".Fields-2.", 62, 8, 0, 18),
		createField("double", ".CompFields-3.", 62, 8, 0, 18),
		createField("RBI-NUMBER-S96SLS (0)", ".CompFields..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
		createField("RBI-NUMBER-S96SLS (0)", ".Fields-2..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
		createField("RBI-NUMBER-S96SLS (0)", ".CompFields-3..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
		createField("RBI-NUMBER-S96DISP (0)", ".CompFields..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
		createField("RBI-NUMBER-S96DISP (0)", ".Fields-2..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
		createField("RBI-NUMBER-S96DISP (0)", ".CompFields-3..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
		createField("SFIELD-SEP", ".CompFields..", 83, 10, 2, 9),
		createField("SFIELD-SEP", ".Fields-2..", 83, 10, 2, 9),
		createField("SFIELD-SEP", ".CompFields-3..", 83, 10, 2, 9),
		createField("REN-RETURNED-YEAR", ".CompFields.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
		createField("REN-RETURNED-YEAR", ".Fields-2.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
		createField("REN-RETURNED-YEAR", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
		createField("REN-RETURNED-MONTH", ".CompFields.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
		createField("REN-RETURNED-MONTH", ".Fields-2.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
		createField("REN-RETURNED-MONTH", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
		createField("REN-RETURNED-DAY", ".CompFields.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
		createField("REN-RETURNED-DAY", ".Fields-2.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
		createField("REN-RETURNED-DAY", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
		createField("REN-CAR-TYPE", ".CompFields.88-levels.", 99, 1, 0, 0),
		createField("REN-CAR-TYPE", ".Fields-2.88-levels.", 99, 1, 0, 0),
		createField("REN-CAR-TYPE", ".CompFields-3.88-levels.", 99, 1, 0, 0),
		createField("REN-DAYS-RENTED", ".CompFields.88-levels.", 100, 2, 0, 25),
		createField("REN-DAYS-RENTED", ".Fields-2.88-levels.", 100, 2, 0, 25),
		createField("REN-DAYS-RENTED", ".CompFields-3.88-levels.", 100, 2, 0, 25),
		createField("Store-Num", ".CompFields.occurs-items.", 102, 4, 0, 25),
		createField("Store-Name", ".CompFields.occurs-items.", 106, 30, 0, 0),
		createField("Department-Num (0)", ".CompFields.occurs-items.Department-Dtls.", 136, 4, 0, 25),
		createField("Department-name (0)", ".CompFields.occurs-items.Department-Dtls.", 140, 20, 0, 0),
		createField("keycode (0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 160, 8, 0, 25),
		createField("Qty (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 168, 6, 0, 6),
		createField("Price (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 174, 9, 2, 6),
		createField("trans-type (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 183, 1, 0, 0),
		createField("keycode (0, 1)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 184, 8, 0, 25),
		createField("Qty (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 192, 6, 0, 6),
		createField("Price (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 198, 9, 2, 6),
		createField("trans-type (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 207, 1, 0, 0),
		createField("Qty (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 208, 6, 0, 6),
		createField("Price (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 214, 9, 2, 6),
		createField("Sku-Count (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 223, 6, 0, 6),
		createField("Department-Num (1)", ".CompFields.occurs-items.Department-Dtls.", 229, 4, 0, 25),
		createField("Department-name (1)", ".CompFields.occurs-items.Department-Dtls.", 233, 20, 0, 0),
		createField("keycode (1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 253, 8, 0, 25),
		createField("Qty (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 261, 6, 0, 6),
		createField("Price (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 267, 9, 2, 6),
		createField("trans-type (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 276, 1, 0, 0),
		createField("keycode (1, 1)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 277, 8, 0, 25),
		createField("Qty (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 285, 6, 0, 6),
		createField("Price (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 291, 9, 2, 6),
		createField("trans-type (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 300, 1, 0, 0),
		createField("Qty (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 301, 6, 0, 6),
		createField("Price (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 307, 9, 2, 6),
		createField("Sku-Count (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 316, 6, 0, 6),
		createField("keycode (0)", ".CompFields.occurs-items.Orders.", 322, 8, 0, 25),
		createField("Qty (0)", ".CompFields.occurs-items.Orders.", 330, 6, 0, 6),
		createField("keycode (1)", ".CompFields.occurs-items.Orders.", 336, 8, 0, 25),
		createField("Qty (1)", ".CompFields.occurs-items.Orders.", 344, 6, 0, 6),
		createField("array-fld (0)", ".CompFields.occurs-items.", 350, 2, 0, 0),
		createField("array-fld (1)", ".CompFields.occurs-items.", 352, 2, 0, 0),
		createField("array-fld (2)", ".CompFields.occurs-items.", 354, 2, 0, 0),
		createField("data-field", ".CompFields.occurs-items.", 356, 10, 0, 0),
		createField("rc (0)", ".CompFields.occurs-items.array-redef.", 356, 2, 0, 25),
		createField("rc1 (0)", ".CompFields.occurs-items..", 356, 2, 0, 25),
		createField("rc2 (0)", ".CompFields.occurs-items...", 356, 2, 0, 25),
		createField("rc3 (0)", ".CompFields.occurs-items...", 356, 2, 0, 25),
		createField("rc (1)", ".CompFields.occurs-items.array-redef.", 358, 2, 0, 25),
		createField("rc1 (1)", ".CompFields.occurs-items..", 358, 2, 0, 25),
		createField("rc2 (1)", ".CompFields.occurs-items...", 358, 2, 0, 25),
		createField("sep0", ".CompFields.Signed-Comp.", 366, 1, 0, 0),
		createField("Num0", ".CompFields.Signed-Comp.", 367, 2, 0, 35),
		createField("sep1", ".CompFields.Signed-Comp.", 369, 1, 0, 0),
		createField("Num1", ".CompFields.Signed-Comp.", 370, 2, 2, 35),
		createField("sep2", ".CompFields.Signed-Comp.", 372, 1, 0, 0),
		createField("Num2", ".CompFields.Signed-Comp.", 373, 2, 2, 35),
		createField("sep0", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 375, 1, 0, 0),
		createField("Num0", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 376, 2, 0, Type.ftPositiveBinaryBigEndian),
		createField("sep1", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 378, 1, 0, 0),
		createField("Num1", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 379, 2, 2, Type.ftPositiveBinaryBigEndian),
		createField("sep2", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 381, 1, 0, 0),
		createField("Num2", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 382, 2, 2, Type.ftPositiveBinaryBigEndian),
		createField("sep0", ".CompFields.Signed-Comp-3.aa.", 384, 1, 0, 0),
		createField("Num0", ".CompFields.Signed-Comp-3.aa.", 385, 2, 0, 31),
		createField("sep1", ".CompFields.Signed-Comp-3.aa.", 387, 1, 0, 0),
		createField("Num1", ".CompFields.Signed-Comp-3.aa.", 388, 2, 2, 31),
		createField("sep0", ".CompFields.UnSigned-Comp-3.aa.", 390, 1, 0, 0),
		createField("Num0", ".CompFields.UnSigned-Comp-3.aa.", 391, 2, 0, 33),
		createField("sep1", ".CompFields.UnSigned-Comp-3.aa.", 393, 1, 0, 0),
		createField("Num1", ".CompFields.UnSigned-Comp-3.aa.", 394, 3, 2, 33),
		createField("sep0", ".CompFields.Signed-Comp-sync.", 397, 1, 0, 0),
		createField("Num0", ".CompFields.Signed-Comp-sync.", 399, 2, 0, 35),
		createField("sep1", ".CompFields.Signed-Comp-sync.", 401, 1, 0, 0),
		createField("Num1", ".CompFields.Signed-Comp-sync.", 403, 2, 2, 35),
		createField("sep0", ".CompFields.G-Comp-4.aa.", 405, 1, 0, 0),
		createField("Num0", ".CompFields.G-Comp-4.aa.", 406, 2, 0, 35),
		createField("sep1", ".CompFields.G-Comp-4.aa.", 408, 1, 0, 0),
		createField("Num1", ".CompFields.G-Comp-4.aa.", 409, 2, 2, 35),
		createField("Num2", ".CompFields.G-Comp-4.aa.", 411, 2, 2, Type.ftPositiveBinaryBigEndian),
		createField("sep0", ".CompFields.G-Comp-5.aa.", 413, 1, 0, 0),
		createField("Num0", ".CompFields.G-Comp-5.aa.", 414, 2, 0, 35),
		createField("sep1", ".CompFields.G-Comp-5.aa.", 416, 1, 0, 0),
		createField("Num1", ".CompFields.G-Comp-5.aa.", 417, 2, 2, 35),
		createField("Num2", ".CompFields.G-Comp-5.aa.", 419, 2, 2, Type.ftPositiveBinaryBigEndian),
		createField("PIC-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 421, 1, 0, 0),
		createField("PIC-TEST-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 422, 2, 0, 0),
		createField("PIC-TEST-3", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 424, 3, 0, 0),
		createField("PIC-TEST-4", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 427, 1, 0, 32),
		createField("PIC-TEST-5", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 428, 3, 1, 6),
		createField("PIC-TEST-6", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 431, 3, 2, 32),
		createField("PIC-TEST-7", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 434, 8, 0, 0),
		createField("VALUE-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 442, 3, 0, 0),
		createField("VALUE-TEST-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 445, 3, 0, 25),
		createField("VALUE-TEST-3", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 448, 3, 0, 0),
		createField("VALUE-TEST-4", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 451, 3, 0, 25),
		createField("VALUE-TEST-5", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 454, 5, 0, 0),
		createField("DEPENDING-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 459, 1, 0, 25),
		createField("DEPENDING-TEST-2 (0)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 460, 1, 0, 0),
		createField("DEPENDING-TEST-2 (1)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 461, 1, 0, 0),
		createField("DEPENDING-TEST-2 (2)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 462, 1, 0, 0),
		createField("GROUP-WITH-88-LEVEL-ITEM-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-88-LEVEL.", 463, 2, 0, 0),
		createField("GROUP-WITH-88-LEVEL-ITEM-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-88-LEVEL.", 465, 3, 0, 0),
		createField("LOWERCASE-KEYWORDS", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 468, 1, 0, 25),
		createField("lowercase-identifier", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 469, 1, 0, 0),
		createField("MiXeDcAsE-TEST", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 470, 3, 0, 0),
		createField("LOWERCASE-88-VALUE", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 473, 5, 0, 0),
		createField("UNCOMMENTED-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.INDICATOR-COLUMN-SLASH-TEST-GROUP.", 478, 1, 0, 0),
		createField("UNCOMMENTED-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.INDICATOR-COLUMN-SLASH-TEST-GROUP.", 479, 1, 0, 0),
		createField("PIC-TEST-99", ".CompFields.some-more-items.ANOTHER-TOP-LEVEL-ITEM.", 480, 1, 0, 0),
		createField("PIC-TEST-98", ".CompFields.some-more-items.ANOTHER-TOP-LEVEL-ITEM.", 481, 1, 0, 25),

	}, 	
	null, null
	};
	

	private String[] basicXml = {
			  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"Cbl_Line_Test_Record\" COPYBOOK=\"Cbl_Line_Test_Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <ITEMS CopybookPref=\"Cbl_Line_Test_Record\" JRecNaming=\"TRUE\">\n"
			+ "        <item level=\"01\" name=\"Cbl-Line-Test-Record\" position=\"1\" storage-length=\"150\" display-length=\"191\">\n"
			+ "            <item level=\"03\" name=\"Fld-Char\" picture=\"X(10)\" position=\"1\" storage-length=\"10\" display-length=\"10\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Char-right-just\" picture=\"X(10)\" position=\"11\" storage-length=\"10\" display-length=\"10\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Decimal\" picture=\"s9(9)\" usage=\"computational-3\" position=\"21\" storage-length=\"5\" display-length=\"9\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-1\" picture=\"x(8)\" position=\"26\" storage-length=\"8\" display-length=\"8\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-2\" picture=\"x(4)\" position=\"34\" storage-length=\"4\" display-length=\"4\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-3\" picture=\"x(8)\" position=\"38\" storage-length=\"8\" display-length=\"8\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Num-Right-Just\" picture=\"-(7)9\" position=\"46\" storage-length=\"8\" display-length=\"8\" numeric=\"Numeric_Edited\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Num-Right-Just1\" picture=\"9(7)9\" position=\"54\" storage-length=\"8\" display-length=\"8\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Postive-Int\" picture=\"s9(9)\" usage=\"computational\" position=\"62\" storage-length=\"4\" display-length=\"9\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Assummed-Decimal\" picture=\"s9(6)v9999\" position=\"66\" storage-length=\"10\" display-length=\"10\" scale=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Num-2-decimal\" picture=\"-(7)9v99\" position=\"76\" storage-length=\"10\" display-length=\"10\" scale=\"2\" numeric=\"Numeric_Edited\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Decimal-2-digits\" picture=\"s9(7)v99\" usage=\"computational-3\" position=\"86\" storage-length=\"5\" display-length=\"9\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Positive-Int-2-digit\" picture=\"9(7)v99\" usage=\"computational\" position=\"91\" storage-length=\"4\" display-length=\"9\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-4\" picture=\"x(6)\" position=\"95\" storage-length=\"6\" display-length=\"6\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Mainframe-Int\" picture=\"s9(9)\" usage=\"computational\" position=\"101\" storage-length=\"4\" display-length=\"9\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-MainframeInt2decimal\" picture=\"s9(7)v99\" usage=\"computational\" position=\"105\" storage-length=\"4\" display-length=\"9\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-5\" picture=\"x\" position=\"109\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-6\" picture=\"xx\" position=\"110\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-7\" picture=\"x(4)\" position=\"112\" storage-length=\"4\" display-length=\"4\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-8\" picture=\"x(8)\" position=\"116\" storage-length=\"8\" display-length=\"8\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Mainframe-Small-Int\" picture=\"s9(4)\" usage=\"computational\" position=\"124\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Mainframe-Long\" picture=\"s9(15)\" usage=\"computational\" position=\"126\" storage-length=\"8\" display-length=\"15\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Mainframe-Packed-Decimal\" picture=\"s9(5)\" usage=\"computational-3\" position=\"134\" storage-length=\"3\" display-length=\"5\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Mainframe-Packed-DecimalP\" picture=\"s9(4)V9\" usage=\"computational-3\" position=\"137\" storage-length=\"3\" display-length=\"5\" scale=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Zoned\" picture=\"s99\" position=\"140\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Fld-Zoned-decimalp\" picture=\"s99v99\" position=\"142\" storage-length=\"4\" display-length=\"4\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"03\" name=\"Filler-3\" picture=\"x(5)\" position=\"146\" storage-length=\"5\" display-length=\"5\"/>\n"
			+ "        </item>\n"
			+ "    </ITEMS>\n"
			+ "</RECORD>"
			, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
					+ "<RECORD RECORDNAME=\"DTAR020\" COPYBOOK=\"DTAR020\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <ITEMS CopybookPref=\"DTAR020\" JRecNaming=\"TRUE\">\n"
			+ "        <item level=\"03\" name=\"DTAR020-KCODE-STORE-KEY\" position=\"1\" storage-length=\"10\" display-length=\"11\">\n"
			+ "            <item level=\"05\" name=\"DTAR020-KEYCODE-NO\" picture=\"X(08)\" position=\"1\" storage-length=\"8\" display-length=\"8\"/>\n"
			+ "            <item level=\"05\" name=\"DTAR020-STORE-NO\" picture=\"S9(03)\" usage=\"computational-3\" position=\"9\" storage-length=\"2\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"DTAR020-DATE\" picture=\"S9(07)\" usage=\"computational-3\" position=\"11\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "        <item level=\"03\" name=\"DTAR020-DEPT-NO\" picture=\"S9(03)\" usage=\"computational-3\" position=\"15\" storage-length=\"2\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "        <item level=\"03\" name=\"DTAR020-QTY-SOLD\" picture=\"S9(9)\" usage=\"computational-3\" position=\"17\" storage-length=\"5\" display-length=\"9\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "        <item level=\"03\" name=\"DTAR020-SALE-PRICE\" picture=\"S9(9)V99\" usage=\"computational-3\" position=\"22\" storage-length=\"6\" display-length=\"11\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "    </ITEMS>\n"
			+ "</RECORD>"
			, null
			,  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"FCUSDAT\" COPYBOOK=\"FCUSDAT\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <ITEMS CopybookPref=\"FCUSDAT\" JRecNaming=\"TRUE\">\n"
			+ "        <item level=\"01\" name=\"CUSTOMER-DATA\" position=\"1\" storage-length=\"183\" display-length=\"223\">\n"
			+ "            <item level=\"05\" name=\"CUSTOMER-ID\" picture=\"9(6)\" position=\"1\" storage-length=\"6\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"05\" name=\"PERSONAL-DATA\" position=\"7\" storage-length=\"48\" display-length=\"48\">\n"
			+ "                <item level=\"10\" name=\"CUSTOMER-NAME\" picture=\"X(20)\" position=\"7\" storage-length=\"20\" display-length=\"20\"/>\n"
			+ "                <item level=\"10\" name=\"CUSTOMER-ADDRESS\" picture=\"X(20)\" position=\"27\" storage-length=\"20\" display-length=\"20\"/>\n"
			+ "                <item level=\"10\" name=\"CUSTOMER-PHONE\" picture=\"X(8)\" position=\"47\" storage-length=\"8\" display-length=\"8\"/>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"TRANSACTIONS\" position=\"55\" storage-length=\"129\" display-length=\"169\">\n"
			+ "                <item level=\"10\" name=\"TRANSACTION-NBR\" picture=\"9(9)\" usage=\"computational\" position=\"55\" storage-length=\"4\" display-length=\"9\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                <item level=\"10\" name=\"TRANSACTION\" occurs=\"5\" occurs-min=\"0\" depending-on=\"TRANSACTION-NBR\" position=\"59\" storage-length=\"25\" display-length=\"32\">\n"
			+ "                    <item level=\"15\" name=\"TRANSACTION-DATE\" picture=\"X(8)\" position=\"59\" storage-length=\"8\" display-length=\"8\" redefined=\"true\"/>\n"
			+ "                    <item level=\"15\" name=\"FILLER\" position=\"59\" storage-length=\"8\" display-length=\"8\" redefines=\"TRANSACTION-DATE\">\n"
			+ "                        <item level=\"20\" name=\"TRANSACTION-DAY\" picture=\"X(2)\" position=\"59\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                        <item level=\"20\" name=\"FILLER\" picture=\"X\" position=\"61\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                        <item level=\"20\" name=\"TRANSACTION-MONTH\" picture=\"X(2)\" position=\"62\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                        <item level=\"20\" name=\"FILLER\" picture=\"X\" position=\"64\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                        <item level=\"20\" name=\"TRANSACTION-YEAR\" picture=\"X(2)\" position=\"65\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                    </item>\n"
			+ "                    <item level=\"15\" name=\"TRANSACTION-AMOUNT\" picture=\"S9(13)V99\" usage=\"computational-3\" position=\"67\" storage-length=\"8\" display-length=\"15\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                    <item level=\"15\" name=\"TRANSACTION-COMMENT\" picture=\"X(9)\" position=\"75\" storage-length=\"9\" display-length=\"9\"/>\n"
			+ "                </item>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "    </ITEMS>\n"
			+ "</RECORD>"
			
			, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"RBIVCopy\" COPYBOOK=\"RBIVCopy\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <ITEMS CopybookPref=\"RBIVCopy\" JRecNaming=\"TRUE\">\n"
			+ "        <item level=\"01\" name=\"RBI-RECORD-T1\" position=\"1\" storage-length=\"124\" display-length=\"128\">\n"
			+ "            <item level=\"05\" name=\"RBI-TEXT-1\" picture=\"X(50)\" position=\"1\" storage-length=\"50\" display-length=\"50\"/>\n"
			+ "            <item level=\"05\" name=\"RBI-QTY\" picture=\"9(03)\" position=\"51\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"05\" name=\"RBI-REPETITIVE-AREA\" position=\"54\" storage-length=\"21\" display-length=\"25\">\n"
			+ "                <item level=\"10\" name=\"RBI-REPEAT\" occurs=\"1\" position=\"54\" storage-length=\"21\" display-length=\"25\">\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96SLS\" picture=\"S9(06)\" position=\"54\" storage-length=\"7\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" sign-clause=\"leading_separate\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96DISP\" picture=\"S9(06)\" position=\"61\" storage-length=\"6\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96CMP3\" picture=\"S9(06)\" usage=\"computational-3\" position=\"67\" storage-length=\"4\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96CMP\" picture=\"S9(06)\" usage=\"computational\" position=\"71\" storage-length=\"4\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                </item>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"RBI-TEXTE-2\" picture=\"X(50)\" position=\"75\" storage-length=\"50\" display-length=\"50\"/>\n"
			+ "        </item>\n"
			+ "        <item level=\"01\" name=\"RBI-RECORD-T2\" position=\"1\" storage-length=\"145\" display-length=\"153\">\n"
			+ "            <item level=\"05\" name=\"RBI-TEXT-1\" picture=\"X(50)\" position=\"1\" storage-length=\"50\" display-length=\"50\"/>\n"
			+ "            <item level=\"05\" name=\"RBI-QTY\" picture=\"9(03)\" position=\"51\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "            <item level=\"05\" name=\"RBI-REPETITIVE-AREA\" position=\"54\" storage-length=\"42\" display-length=\"50\">\n"
			+ "                <item level=\"10\" name=\"RBI-REPEAT\" occurs=\"2\" position=\"54\" storage-length=\"21\" display-length=\"25\">\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96SLS\" picture=\"S9(06)\" position=\"54\" storage-length=\"7\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" sign-clause=\"leading_separate\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96DISP\" picture=\"S9(06)\" position=\"61\" storage-length=\"6\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96CMP3\" picture=\"S9(06)\" usage=\"computational-3\" position=\"67\" storage-length=\"4\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                    <item level=\"15\" name=\"RBI-NUMBER-S96CMP\" picture=\"S9(06)\" usage=\"computational\" position=\"71\" storage-length=\"4\" display-length=\"6\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                </item>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"RBI-TEXTE-2\" picture=\"X(50)\" position=\"96\" storage-length=\"50\" display-length=\"50\"/>\n"
			+ "        </item>\n"
			+ "    </ITEMS>\n"
			+ "</RECORD>",
			null,
			null,
			null,
			null
	};

	
	private int[] DIALECTS = {
			ICopybookDialects.FMT_MAINFRAME, ICopybookDialects.FMT_GNU_COBOL,
			ICopybookDialects.FMT_FUJITSU
	};
	
	private int[] fileStructures = {
			Constants.IO_FIXED_LENGTH, Constants.IO_VB
	};
	
	public void testCopybookFields() throws IOException, XMLStreamException, FactoryConfigurationError {
		tstCopybooks(basicCopybooks, basicXml);
	}
	
	void tstCopybooks(String[] basicCopybooks, String[] expectedXml) throws IOException, XMLStreamException, FactoryConfigurationError {
		
		int idx = 0;
		for (String cpy : basicCopybooks) {
			for (int dialect : DIALECTS) {
				for (int fs : fileStructures) {
					doCopybookTst(expectedXml[idx], expectFieldsForCopybooks[idx], cpy, dialect, fs);
				}
			}
			idx += 1;
		}
	}
	
	public void testOneCopybook() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {
		int idx = 6;
		doCopybookTst(basicXml[idx], 
				null /*expectFieldsForCopybooks[5]*/, basicCopybooks[idx],  
				ICopybookDialects.FMT_MAINFRAME, Constants.IO_FIXED_LENGTH);
	}

	/**
	 * @param expectedXml
	 * @param idx
	 * @param cpy
	 * @param dialect
	 * @param fs
	 * @throws XMLStreamException
	 * @throws UnsupportedEncodingException
	 * @throws FactoryConfigurationError
	 * @throws IOException
	 */
	private void doCopybookTst(String expectedXml, FieldDetail[] expectedFields, String cpy, int dialect, int fs)
			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {
		byte[] cpyBytes = TestCommonCode.loadFile(TstConstants.COBOL_DIRECTORY + cpy + ".cbl");
		boolean hasConds = DTAR107.equals(cpy) 
						|| BIT_OF_EVERYTHING_01.equals(cpy) || BIT_OF_EVERYTHING_02.equals(cpy);
		ICobolIOBuilder iob1 = JRecordInterface1.COBOL
			.newIOBuilder(new ByteArrayInputStream(cpyBytes), cpy)
				.setFileOrganization(fs)
				.setDialect(dialect);
		checkCobolIoBuilder(cpy, hasConds, iob1, fs,
				dialect == ICopybookDialects.FMT_MAINFRAME && fs == Constants.IO_FIXED_LENGTH
					? expectedXml
					: null);
		
		if (dialect == ICopybookDialects.FMT_MAINFRAME 
		&& (! BIT_OF_EVERYTHING_01.equals(cpy)) && (! OCCURS_DEPENDING.equals(cpy))) {
			checkLayoutFields(expectedFields, iob1, cpy);
		} /*else if (BIT_OF_EVERYTHING_01.equals(cpy)) {
			checkLayoutFields(null, iob1, cpy);
		}*/
	}

	/**
	 * @param cpy
	 * @param hasConds
	 * @param iob1
	 * @throws XMLStreamException
	 * @throws UnsupportedEncodingException
	 * @throws FactoryConfigurationError
	 * @throws IOException
	 */
	private void checkCobolIoBuilder(String cpy, boolean hasConds, ICobolIOBuilder iob1, int fs, String xmlstr)
			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {
		StringWriter sw1 = new StringWriter(1000);
		StringWriter sw2 = new StringWriter(1000);
		
		JRecordInterface1.SCHEMA_XML
				.setIndentXml(! hasConds)
				.export(sw1, iob1.getExternalRecord());
		
		String xmlString1 = sw1.toString();
		IFileIOBuilder iob2 = JRecordInterface1.SCHEMA_XML.newIOBuilder(new StringReader(xmlString1), cpy);
		
		JRecordInterface1.SCHEMA_XML
				.setIndentXml(! hasConds)
				.export(sw2, iob2.getExternalRecord());
		String xmlString2 = sw2.toString();
		
		if (hasConds) {
			xmlString1 = dropCondition(xmlString1);
		}
		assertEquals(cpy, xmlString1, xmlString2);

		//System.out.println(xmlString1);
		compare(iob1, iob2, fs);
		
		if (xmlstr == null) {
			
		} else if (xmlstr.length() == 0) {
			System.out.println("\n\t" + cpy + ":\n");
			System.out.println(xmlString1);
		} else {
			assertEquals(xmlstr, xmlString1);
		}
	}

	/**
	 * Compare the field details on 2 iobs
	 * @param iob1
	 * @return
	 * @throws IOException
	 */
	private void compare(ICobolIOBuilder iob1, IFileIOBuilder iob2, int fs) throws IOException {
		ExternalRecord rec1 = iob1	.getExternalRecord();
		ExternalRecord rec2 = iob2	.getExternalRecord();

		LayoutDetail schema1 = rec1	.asLayoutDetail();
		LayoutDetail schema2 = rec2	.asLayoutDetail();
		
		assertEquals(fs, rec1.getFileStructure());
		assertEquals(fs, rec2.getFileStructure());
		assertEquals(fs, schema1.getFileStructure());
		assertEquals(fs, schema2.getFileStructure());
		
		int size = rec1.getNumberOfRecordFields();
		ExternalField[] xFields1 = rec1.getRecordFields();
		ExternalField[] xFields2 = rec2.getRecordFields();
		
		TypeManager typeMgr = TypeManager.getInstance();
		CharsetType charsetType = typeMgr.getCharsetType(rec1.getFontName());
//		FieldDetail[] lFields = new FieldDetail[size];
		
		assertEquals(size, rec2.getNumberOfRecordFields());
		for (int i = 0; i < size; i++) {
			assertEquals(xFields1[i].getDecimal(), xFields2[i].getDecimal());
			assertEquals(xFields1[i].getGroup(),   xFields2[i].getGroup());
			assertEquals(xFields1[i].getLen(),     xFields2[i].getLen());
			assertEquals(xFields1[i].getName(),    xFields2[i].getName());
			assertEquals(xFields1[i].getPos(),     xFields2[i].getPos());
			assertEquals(xFields1[i].getType(),    xFields2[i].getType());  
			
			if (xFields1[i].getDependOnDtls() != null) {
				assertEquals(xFields1[i].getDependOnDtls().firstIdx, xFields2[i].getDependOnDtls().firstIdx);
				assertEquals(xFields1[i].getDependOnDtls().index,    xFields2[i].getDependOnDtls().index);
//				assertEquals(xFields1[i].getDependOnDtls().firstIdx, xFields2[i].getDependOnDtls().firstIdx);
			}
		}
		size = schema1.getRecord(0).getFieldCount();
		assertEquals(size, schema2.getRecord(0).getFieldCount());
		
		for (int i = 0; i < size ; i++) {
			FieldDetail field1 = schema1.getField(0, i);
			FieldDetail field2 = schema2.getField(0, i);
			
			assertEquals(field1.getFontName(),   field2.getFontName());

			compareLayoutFields(field1, field2);
		}
		
		if (size != rec1.getNumberOfRecordFields()) {
			assertEquals(size, rec1.getNumberOfRecordFields());
		}
		Arrays.sort(xFields1, new Comparator<ExternalField>() {
			@Override public int compare(ExternalField o1, ExternalField o2) {
				return Integer.compare(o1.getPos(), o2.getPos());
			}
		});
		for (int i = 0; i < size; i++) {
			FieldDetail field1 = schema1.getField(0, i);
			
			if (! xFields1[i].getName().equals(field1.getName())) {
				assertEquals(xFields1[i].getName(), xFields1[i].getGroup(),   field1.getGroupName());
			}
			assertEquals(xFields1[i].getName(), field1.getName());
			assertEquals(xFields1[i].getName(), xFields1[i].getDecimal(), field1.getDecimal());
			assertEquals(xFields1[i].getName(), xFields1[i].getGroup(),  field1.getGroupName());
			
			assertEquals(xFields1[i].getLen(),  field1.getLen());
			assertEquals(xFields1[i].getPos(),  field1.getPos());
			assertEquals(xFields1[i].getName() + " " + i, 
					typeMgr.getShortType(xFields1[i].getType(), xFields1[i].getLen(), charsetType),
					field1.getType());  
		}
	}

	/**
	 * @param field1
	 * @param field2
	 */
	private void compareLayoutFields(FieldDetail field1, FieldDetail field2) {
		TypeManager typeMgr = TypeManager.getInstance();
		assertEquals(field1.getName(),       field2.getName());
		assertEquals(field1.getLookupName(), field2.getLookupName());
		assertEquals(field1.getDecimal(),    field2.getDecimal());;
		assertEquals(field1.getEnd(),        field2.getEnd()    );
		assertEquals(field1.getGroupName(),  field2.getGroupName());
		assertEquals(field1.getLen(),        field2.getLen());
		assertEquals(field1.getPos(),        field2.getPos());
		
		if (field1.getType() != field2.getType()) {
			int shortType = typeMgr.getShortType(field1.getType(), field1.getLen(), field2.getFontName());
			assertEquals(
					shortType,       
					field2.getType());
		}
		assertEquals(field1.getName(),       field2.getName());
	}
	
	/**
	 * Drop[ conditions from Xml
	 * @param xmlStr
	 * @return
	 */
	public static String dropCondition(String xmlStr) {
		String xmlStrLC = xmlStr.toLowerCase();
		StringBuilder b = new StringBuilder(xmlStr.length());
		int nextPos = xmlStrLC.indexOf(CONDITION);
		int posEnd;
		int i = 0;
		while (nextPos >= 0 && i < xmlStr.length()) {
			if (i < nextPos) {
				b.append(xmlStr.charAt(i++));
			} else {
				while (i < xmlStr.length() && xmlStr.charAt(i++) != '>') {}
				
				posEnd = xmlStrLC.indexOf(END_CONDITION, i);
				nextPos = xmlStr.indexOf(CONDITION, i);
				if (nextPos < 0 || (posEnd >= 0 && posEnd < nextPos)) {
					nextPos = posEnd;
				} 
			}
		}
		for (int j = i; j < xmlStr.length(); j++) {
			b.append(xmlStr.charAt(j));
		}
		
		Conversion.replace(b, "\"></item>", "\"/>" ); 
		return b.toString();
	}
	
	
	String cblSignClause
		= "        01  Record.\n"
		+ "            03 pic-x                  pic x(4).\n"
		+ "            03 unsigned-1             pic 9(4).\n"
		+ "            03 signed-1               pic s9(4).\n"
		+ "            03 signed-leading         pic s9(4) sign leading.\n"
		+ "            03 signed-trailing        pic s9(4) sign trailing.\n"
		+ "            03 signed-sep-leading     pic s9(4) sign leading separate.\n"
		+ "            03 signed-sep-trailing    pic s9(4) sign trailing separate.\n";
	
	FieldDetail signClauseFields[] = {
			createField("pic-x", ".Record.",1, 4, 0, 0),
			createField("unsigned-1", ".Record.",5, 4, 0, 25),
			createField("signed-1", ".Record.",9, 4, 0, 32),
			createField("signed-leading", ".Record.",13, 4, 0, 32),
			createField("signed-trailing", ".Record.",17, 4, 0, 32),
			createField("signed-sep-leading", ".Record.",21, 5, 0, 9),
			createField("signed-sep-trailing", ".Record.",26, 5, 0, 10),
	};
	String signXml
		= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
		+ "    <ITEMS CopybookPref=\"Record\" JRecNaming=\"TRUE\">\n"
		+ "        <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
		+ "            <item level=\"03\" name=\"pic-x\" picture=\"x(4)\" position=\"1\" storage-length=\"4\" display-length=\"4\"/>\n"
		+ "            <item level=\"03\" name=\"unsigned-1\" picture=\"9(4)\" position=\"5\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "            <item level=\"03\" name=\"signed-1\" picture=\"s9(4)\" position=\"9\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "            <item level=\"03\" name=\"signed-leading\" picture=\"s9(4)\" position=\"13\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" sign-clause=\"leading\"/>\n"
		+ "            <item level=\"03\" name=\"signed-trailing\" picture=\"s9(4)\" position=\"17\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" sign-clause=\"trailing\"/>\n"
		+ "            <item level=\"03\" name=\"signed-sep-leading\" picture=\"s9(4)\" position=\"21\" storage-length=\"5\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" sign-clause=\"leading_separate\"/>\n"
		+ "            <item level=\"03\" name=\"signed-sep-trailing\" picture=\"s9(4)\" position=\"26\" storage-length=\"5\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" sign-clause=\"trailing_separate\"/>\n"
		+ "        </item>\n"
		+ "    </ITEMS>\n"
		+ "</RECORD>";

	public void testSignClause() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {
		checkFields(cblSignClause, signClauseFields, signXml);
	}

	

	String cblComp
		= "        01  Record.\n"
		+ "            05 grp-1.\n"
		+ "               10 pic-9                   pic s9(4).\n"
		+ "               10 pic-9e    comp-1.\n"
		+ "               10 pic-9f    comp-2.\n"
		+ "            05 grp-2.\n"
		+ "               10 pic-9a                  pic s9(4) comp.\n"
		+ "               10 pic-9b                  pic s9(4) comp-3.\n"
		+ "               10 pic-9c                  pic s9(4) comp-4.\n"
		+ "               10 pic-9d                  pic s9(4) comp-5.\n"
		+ "               10 pic-9e                  pic s9(4) comp-6.\n"
		+ "               10 pic-9f                  pic s9(4) binary.\n"
		+ "            05 grp-3.\n"
		+ "               10 pic-9a                  pic s9(4) usage comp.\n"
		+ "               10 pic-9b                  pic s9(4) usage comp-3.\n"
		+ "               10 pic-9c                  pic s9(4) usage comp-4.\n"
		+ "               10 pic-9d                  pic s9(4) usage comp-5.\n"
		+ "               10 pic-9e                  pic s9(4) usage comp-6.\n"
		+ "               10 pic-9f                  pic s9(4) usage binary.\n"
		+ "            05 grp-4.\n"
		+ "               10 pic-9a                  pic s9(4) computational.\n"
		+ "               10 pic-9b                  pic s9(4) computational-3.\n"
		+ "               10 pic-9c                  pic s9(4) computational-4.\n"
		+ "               10 pic-9d                  pic s9(4) computational-5.\n"
		+ "               10 pic-9e                  pic s9(4) computational-6.\n"
		+ "               10 pic-9f                  pic s9(4) binary.\n"
		;

	FieldDetail usageFields[] = {
			createField("pic-9",  ".Record.grp-1.",1, 4, 0, 32),
			createField("pic-9e", ".Record.grp-1.",5, 4, 0, 17),
			createField("pic-9f", ".Record.grp-1.",9, 8, 0, 18),
			createField("pic-9a", ".Record.grp-2.",17, 2, 0, 35),
			createField("pic-9b", ".Record.grp-2.",19, 3, 0, 31),
			createField("pic-9c", ".Record.grp-2.",22, 2, 0, 35),
			createField("pic-9d", ".Record.grp-2.",24, 2, 0, 35),
			createField("pic-9e", ".Record.grp-2.",26, 3, 0, 35),
			createField("pic-9f", ".Record.grp-2.",29, 2, 0, 35),
			createField("pic-9a", ".Record.grp-3.",31, 2, 0, 35),
			createField("pic-9b", ".Record.grp-3.",33, 3, 0, 31),
			createField("pic-9c", ".Record.grp-3.",36, 2, 0, 35),
			createField("pic-9d", ".Record.grp-3.",38, 2, 0, 35),
			createField("pic-9e", ".Record.grp-3.",40, 3, 0, 35),
			createField("pic-9f", ".Record.grp-3.",43, 2, 0, 35),
			createField("pic-9a", ".Record.grp-4.",45, 2, 0, 35),
			createField("pic-9b", ".Record.grp-4.",47, 3, 0, 31),
			createField("pic-9c", ".Record.grp-4.",50, 2, 0, 35),
			createField("pic-9d", ".Record.grp-4.",52, 2, 0, 35),
			createField("pic-9e", ".Record.grp-4.",54, 3, 0, 35),
			createField("pic-9f", ".Record.grp-4.",57, 2, 0, 35),	
	};
	String usageXml
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <ITEMS CopybookPref=\"Record\" JRecNaming=\"TRUE\">\n"
				+ "        <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"58\" display-length=\"104\">\n"
				+ "            <item level=\"05\" name=\"grp-1\" position=\"1\" storage-length=\"16\" display-length=\"32\">\n"
				+ "                <item level=\"10\" name=\"pic-9\" picture=\"s9(4)\" position=\"1\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9e\" usage=\"computational-1\" position=\"5\" storage-length=\"4\" display-length=\"10\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9f\" usage=\"computational-2\" position=\"9\" storage-length=\"8\" display-length=\"18\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "            </item>\n"
				+ "            <item level=\"05\" name=\"grp-2\" position=\"17\" storage-length=\"14\" display-length=\"24\">\n"
				+ "                <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"17\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"19\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"22\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"24\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"26\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"29\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "            </item>\n"
				+ "            <item level=\"05\" name=\"grp-3\" position=\"31\" storage-length=\"14\" display-length=\"24\">\n"
				+ "                <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"31\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"33\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"36\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"38\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"40\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"43\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "            </item>\n"
				+ "            <item level=\"05\" name=\"grp-4\" position=\"45\" storage-length=\"14\" display-length=\"24\">\n"
				+ "                <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"45\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"47\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"50\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"52\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"54\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"57\" storage-length=\"2\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "            </item>\n"
				+ "        </item>\n"
				+ "    </ITEMS>\n"
				+ "</RECORD>";
	

	public void testUsage() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {
		checkFields(cblComp, usageFields, usageXml);
	}

	
	
	static String cblJustified
			=  "        01  Record.\n"
			+ "            03 pic-x-1                 pic x(3).\n"
			+ "            03 pic-x-Justified         pic x(3) justified.\n"
			+ "            03 pic-x-Justified-right   pic x(3) justified right.\n";
		

	FieldDetail justifiedFields[] = {
			createField("pic-x-1", ".Record.",1, 3, 0, 0),
			createField("pic-x-Justified", ".Record.",4, 3, 0, 1),
			createField("pic-x-Justified-right", ".Record.",7, 3, 0, 1),
	};
	
	String justifiedXml
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <ITEMS CopybookPref=\"Record\" JRecNaming=\"TRUE\">\n"
				+ "        <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"9\" display-length=\"9\">\n"
				+ "            <item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "            <item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "            <item level=\"03\" name=\"pic-x-Justified-right\" picture=\"x(3)\" justified=\"right\" position=\"7\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "        </item>\n"
				+ "    </ITEMS>\n"
				+ "</RECORD>";

	public void testJustified() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {		
		checkFields(cblJustified, justifiedFields, justifiedXml);
	}

	
	String cblDecimal
				= "          03 Num-1          pic s9.\n"
				+ "          03 Num-2          pic s9V9.\n"
				+ "          03 Num-3          pic s9V99.\n"
				+ "          03 Num-4          pic s9V999.\n"
				+ "          03 Num-5          pic s9V9999.\n"
				+ "          03 Num-6          pic s9V99999.\n";
	FieldDetail decimalFields[] = {
			createField("Num-1", "",1, 1, 0, 32),
			createField("Num-2", "",2, 2, 1, 32),
			createField("Num-3", "",4, 3, 2, 32),
			createField("Num-4", "",7, 4, 3, 32),
			createField("Num-5", "",11, 5, 4, 32),
			createField("Num-6", "",16, 6, 5, 32),
	};
	String decimalXml
		="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
		+ "    <ITEMS CopybookPref=\"Record\" JRecNaming=\"TRUE\">\n"
		+ "        <item level=\"03\" name=\"Num-1\" picture=\"s9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-2\" picture=\"s9V9\" position=\"2\" storage-length=\"2\" display-length=\"2\" scale=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-3\" picture=\"s9V99\" position=\"4\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-4\" picture=\"s9V999\" position=\"7\" storage-length=\"4\" display-length=\"4\" scale=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-5\" picture=\"s9V9999\" position=\"11\" storage-length=\"5\" display-length=\"5\" scale=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-6\" picture=\"s9V99999\" position=\"16\" storage-length=\"6\" display-length=\"6\" scale=\"5\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "    </ITEMS>\n"
		+ "</RECORD>";

	public void testDecimal() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {		
		checkFields(cblDecimal, decimalFields, decimalXml);
	}
	
	String cblNumeric
		= "          03 Num-1          pic s9V99.\n"
		+ "          03 Num-2          pic s99V99.\n"
		+ "          03 Num-3          pic s999V99.\n"
		+ "          03 Num-4          pic s9999V99.\n"
		+ "          03 Num-5          pic s99999V99.\n"
		+ "          03 Num-6          pic s999999V99.\n";
	FieldDetail numFields[] = {
			createField("Num-1", "",  1, 1, 0, 32),
			createField("Num-2", "",  2, 2, 1, 32),
			createField("Num-3", "",  4, 3, 2, 32),
			createField("Num-4", "",  7, 4, 3, 32),
			createField("Num-5", "", 11, 5, 4, 32),
			createField("Num-6", "", 16, 6, 5, 32),
	};
	
	String numXml
		= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
		+ "    <ITEMS CopybookPref=\"Record\" JRecNaming=\"TRUE\">\n"
		+ "        <item level=\"03\" name=\"Num-1\" picture=\"s9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-2\" picture=\"s9V9\" position=\"2\" storage-length=\"2\" display-length=\"2\" scale=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-3\" picture=\"s9V99\" position=\"4\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-4\" picture=\"s9V999\" position=\"7\" storage-length=\"4\" display-length=\"4\" scale=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-5\" picture=\"s9V9999\" position=\"11\" storage-length=\"5\" display-length=\"5\" scale=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "        <item level=\"03\" name=\"Num-6\" picture=\"s9V99999\" position=\"16\" storage-length=\"6\" display-length=\"6\" scale=\"5\" numeric=\"COBOL_NUMERIC\"/>\n"
		+ "    </ITEMS>\n"
		+ "</RECORD>";

	public void testNum() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {		
		checkFields(cblDecimal, numFields, numXml);
	}

	
//	String cblFillerAtEnd
//		= "          03 Num-1          pic s9V99.\n"
//		+ "          03 Num-2          pic s99V99.\n";
//	String cblFillerAtEnd1
//		= cblFillerAtEnd
//		+ "          03 filler         pic x(40).";
//	String cblFillerAtEnd2
//			= cblFillerAtEnd
//			+ "          03             pic x(40).";
//	FieldDetail fillerAtEndFields1[] = {
//			createField("Num-1", "", 1, 3, 2, 32),
//			createField("Num-2", "", 4, 4, 2, 32),
//			createField("filler", "", 8, 40, 0, 0),
//	};
//	FieldDetail fillerAtEndFields2[] = {
//			createField("Num-1", "", 1, 3, 2, 32),
//			createField("Num-2", "", 4, 4, 2, 32),
//			createField("", "", 8, 40, 0, 0),
//	};
//	
//	public void testFillerAtEnd() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {		
//		checkFields(cblFillerAtEnd1, fillerAtEndFields1, fillerAtEndXml("filler"));
//		checkFields(cblFillerAtEnd2, fillerAtEndFields2, fillerAtEndXml(""));
//	}
//
//	private String fillerAtEndXml(String name) {
//		return 
//				  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
//				+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
//				+ "    <ITEMS CopybookPref=\"Record\">\n"
//				+ "        <item level=\"03\" name=\"Num-1\" picture=\"s9V99\" position=\"1\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
//				+ "        <item level=\"03\" name=\"Num-2\" picture=\"s99V99\" position=\"4\" storage-length=\"4\" display-length=\"4\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
//				+ "        <item level=\"03\" name=\"" + name + "\" picture=\"x(40)\" position=\"8\" storage-length=\"40\" display-length=\"40\"/>\n"
//				+ "    </ITEMS>\n"
//				+ "</RECORD>";
//	}
	
	
	public void testFillerAtEnd1() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {
		FillerCheck[] fillerChecks = {

				new FillerCheck("filler", false, false),
				new FillerCheck("filler", true,  false),
				new FillerCheck("",       false, false),
				new FillerCheck("",       true,  false),
				new FillerCheck("FILLER", false, false),
				new FillerCheck("Filler", true,  false),
				new FillerCheck("FiLLeR", true,  false),
				
				new FillerCheck("filler", false, true),
				new FillerCheck("filler", true,  true),
				new FillerCheck("",       false, true),
				new FillerCheck("",       true,  true),
				new FillerCheck("FILLER", false, true),
		};
		for (FillerCheck c : fillerChecks) {
			checkFields(c.copybook, c.fields, c.xml, c.name, c.keepFillers, false);
		}
	}
	
	public void testOptions() throws UnsupportedEncodingException, XMLStreamException, FactoryConfigurationError, IOException {
		CpyOptions[] opts = {
				new CpyOptions("RECORD", true),
				new CpyOptions("RECORD", false),
		};
		
		for (CpyOptions c : opts) {
			System.out.println(c.name + " " + c.dropCopybook);
			checkFields(c.copybook, c.fields, c.xml, c.name, false, c.dropCopybook);
		}
	}

	/**
	 * @param iob
	 * @param expectedFields
	 * @throws XMLStreamException
	 * @throws UnsupportedEncodingException
	 * @throws FactoryConfigurationError
	 * @throws IOException
	 */
	private void checkFields(String cblStr, FieldDetail[] expectedFields, String expectedXml)
			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {
		checkFields(cblStr, expectedFields, expectedXml, "", false, false);
	}
//	
//	@SuppressWarnings("deprecation")
//	private void checkFields(String cblStr, FieldDetail[] expectedFields, String expectedXml, String cpy, boolean keepFillers)
//			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {
//		checkFields(cblStr, expectedFields, expectedXml, cpy, keepFillers, false, true);
//	}
	
	@SuppressWarnings("deprecation")
	private void checkFields(
			String cblStr, FieldDetail[] expectedFields, String expectedXml, String cpy,
			boolean keepFillers, boolean dropCopybook)
			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {

		ICobolIOBuilder iob = JRecordInterface1.COBOL
				 .newIOBuilder(new StringReader(cblStr), "Record")
				 	.setKeepFillers(keepFillers)
				 	.setDropCopybookNameFromFields(dropCopybook)
				 	.setFileOrganization(Constants.IO_FIXED_LENGTH);

		checkCobolIoBuilder("Record", false, iob, Constants.IO_FIXED_LENGTH, expectedXml);

		checkLayoutFields(expectedFields, iob, cpy);
		
	}

	/**
	 * @param expectedFields
	 * @param iob
	 * @throws IOException
	 */
	private void checkLayoutFields(FieldDetail[] expectedFields, ICobolIOBuilder iob, String cpy) throws IOException {
		LayoutDetail layout = iob.getLayout();
		
		int size = layout.getRecord(0).getFieldCount();

		if (expectedFields != null) {
			assertEquals(cpy, expectedFields.length, size);
			for (int i = 0; i < size; i++) {
				compareLayoutFields(expectedFields[i], layout.getField(0, i));
			}
		} else {
			System.out.println("\t} {");
			for (int i = 0; i < size; i++) {
				FieldDetail f = layout.getField(0, i);
				System.out.println(
						  "\t\tcreateField(\"" + f.getName() + "\", " 
									+ "\"" + f.getGroupName() + "\", "
									+ f.getPos() + ", "
									+ f.getLen() + ", "
									+ f.getDecimal() + ", "
									+ f.getType() + "),");
			}
		}
	}


	private static FieldDetail createField(String name, String groupName, int pos, int len, int decimal, int type) {
		FieldDetail f = FieldDetail.newFixedWidthField(name, type, pos, len, decimal, "");
		f.setGroupName(groupName);
		
		return f;
	}

	
	private static class FillerCheck {
		public final String xml, copybook, name;
		public final FieldDetail[] fields;
		public final boolean keepFillers;
		
		public FillerCheck(String filler, boolean includeFiller, boolean keepFillers) {
			StringBuilder cpyB = new StringBuilder("          03 Num-1          pic s9V99.\n");
			ArrayList<FieldDetail> fieldList = new ArrayList<FieldDetail>();
			int pos = 4;
			StringBuilder xmlB = new StringBuilder();
			String keepFillerStr = keepFillers ? " KeepFiller=\"TRUE\"" : "";
			
			xmlB.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
			    .append("<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n")
			    .append("    <ITEMS CopybookPref=\"Record\"" + keepFillerStr + " JRecNaming=\"TRUE\">\n")
				.append("        <item level=\"03\" name=\"Num-1\" picture=\"s9V99\" position=\"1\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n");
	
			fieldList.add(createField("Num-1", "", 1, 3, 2, 32));
			
			if (includeFiller) {
				if (keepFillers) {
					fieldList.add(createField(filler, "", pos, 4, 0, 0));
				}
				cpyB.append("          03 ") .append(filler) .append("         pic x(4).\n");
				xmlB.append("        <item level=\"03\" name=\"" + filler + "\" picture=\"x(4)\" position=\"" + (pos) +"\" storage-length=\"4\" display-length=\"4\"/>\n");
				
				pos += 4;
			}
			
			fieldList.add(createField("Num-2", "", pos, 4, 2, 32));
			fieldList.add(createField(filler, "", pos + 4, 40, 0, 0));

			cpyB.append("          03 Num-2          pic s99V99.\n")
			    .append("          03 ") .append(filler) .append("         pic x(40).\n");
			xmlB.append("        <item level=\"03\" name=\"Num-2\" picture=\"s99V99\" position=\"" + pos +"\" storage-length=\"4\" display-length=\"4\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n")
				.append("        <item level=\"03\" name=\"" + filler + "\" picture=\"x(40)\" position=\"" + (pos+4) +"\" storage-length=\"40\" display-length=\"40\"/>\n")
				.append("    </ITEMS>\n")
				.append("</RECORD>");
			
			this.copybook = cpyB.toString();
			this.xml = xmlB.toString();
			this.fields = fieldList.toArray(new FieldDetail[fieldList.size()]);
			
			this.name = "c_" + filler + "_" + includeFiller + "_" + keepFillers;
			this.keepFillers = keepFillers;
		}
	};
	
	
	private static class CpyOptions {
		public final String xml, copybook, name;
		public final FieldDetail[] fields;
		public final boolean dropCopybook;
		
		public CpyOptions(String name, boolean dropCopybook) {
			this.dropCopybook = dropCopybook;
			this.copybook
				= "          03  record-Field-1     pic x(30).\n"
				+ "          03  RECORD-Field-2     pic x(10).\n"
				+ "          03  Field-3            pic x(10).\n";
			this.name = name;
			
			String rec1 = "record-";
			String rec2 = "RECORD-";
			String dropCpy = "";
			if (dropCopybook) {
				dropCpy = " DropCopybook=\"TRUE\"";
				if ("record".equalsIgnoreCase(name)) {
					rec1 = "";
					rec2 = "";
				}
			}
			
			FieldDetail[] flds = {
					createField(rec1 + "Field-1", "", 1, 30, 0, 0),
					createField(rec2 + "Field-2", "", 31, 10, 0, 0),
					createField("Field-3", "", 41, 10, 0, 0),
			};
			
			
			fields = flds;
			this.xml 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"Record\" COPYBOOK=\"Record\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Fixed_Length\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <ITEMS CopybookPref=\"Record\"" + dropCpy + " JRecNaming=\"TRUE\">\n"
				+ "        <item level=\"03\" name=\"record-Field-1\" picture=\"x(30)\" position=\"1\" storage-length=\"30\" display-length=\"30\"/>\n"
				+ "        <item level=\"03\" name=\"RECORD-Field-2\" picture=\"x(10)\" position=\"31\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "        <item level=\"03\" name=\"Field-3\" picture=\"x(10)\" position=\"41\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "    </ITEMS>\n"
				+ "</RECORD>";
		}
	};
}
