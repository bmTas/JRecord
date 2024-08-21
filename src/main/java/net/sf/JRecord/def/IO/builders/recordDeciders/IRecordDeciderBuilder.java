package net.sf.JRecord.def.IO.builders.recordDeciders;

import net.sf.JRecord.Details.RecordDecider;

/**
 * Cobol Copybooks do not provide a means to determine which
 * Record applies to a particular Data-Line. Most of the
 * time this does not matter but there are exceptions 
 * (e.g. <b>Constants.IO_CONTINOUS_NO_LINE_MARKER</b>).
 * 
 * <p>One way to tell
 * JRecord / RecordEditor which Record to use for a Data-line
 * is to define a {@link RecordDecider}.
 * 
 * <pre>
 *   iobuilder.setRecordDecider(myRecordDecider);
 * </pre>
 * 
 * <p>While you can write your own {@link RecordDecider}.
 * The RECORD_DECIDER_BUILDER makes it easy to create efficient {@link RecordDecider}
 * using a builder style interface.
 * 
 * <p>For a copybook like:
 * 
 * <pre>
 *       01  Header.
 *           03 Record-Type            Pic x.
 *              88 Header-Rec  value 'H'.
 *              88 Trailer-Rec value 'T'.
 *           ....
 *       01  Detail.
 *           03 Field-1                Pic x.
 *           ....
 *       01  Trailer.
 *           03 Record-TypeT           Pic x.
 *           ....
 * </pre>
 *
 * The Java code
 * 
 *  <pre>
 *     RecordDecider decider = JRecordInterface1.RECORD_DECIDER_BUILDER
 *                                    .singleFieldDeciderBuilder("Record-Type", "Detail")
 *                                        .addRecord("H", "Header")
 *                                        .addRecord("T", "Trailer")
 *                                    .build();
 *    AbstractLineReader r = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name.cbl")
 *                  .setRecordDecider(decider)
 *                  ...                     
 *  </pre>
 * 
 * @author Bruce Martin
 *
 */
public interface IRecordDeciderBuilder {

	/**
	 * Create a {@link RecordDecider} based on a single <i>record-type</i> field with a <i>default record</i> type.
	 * 
	 * <p>You might use it for situations like:
	 *  
	 * <pre>
	 *       01  Header.
	 *           03 Record-Type            Pic x.
	 *              88 Header-Rec  value 'H'.
	 *              88 Detail-Rec  value 'D'.
	 *              88 Trailer-Rec value 'T'.
	 *           ....
	 *       01  Detail.
	 *           03 Record-Type-D          Pic x.
	 *           03 Field-1                Pic x.
	 *           ....
	 *       01  Trailer.
	 *           03 Record-TypeT           Pic x.
	 *           ....
	 * </pre>
	 *
	 * The Java code
	 * 
	 *  <pre>
	 *     RecordDecider decider = JRecordInterface1.RECORD_DECIDER_BUILDER
	 *                                    .singleFieldDeciderBuilder("Record-Type", false)
	 *                                        .addRecord("H", "Header")
	 *                                        .addRecord("D", "Detail")
	 *                                        .addRecord("T", "Trailer")
	 *                                    .build();
	 *     AbstractLineReader r = JRecordInterface1.COBOL
	 *              .newIOBuilder("file-name.cbl")
	 *                  .setRecordDecider(decider)
	 *                  ...                     
	 *  </pre>

	 * @param fieldName record-type field name
	 * @param allowOtherKeyValues allow other Record-Type values (apart from those listed in the RecordDecider definition
	 * 
	 * @return RecordDeciderBuilder for a Single Selection
	 */
	ISingleFieldDeciderBuilder singleFieldDeciderBuilder(String recordTypeFieldName, boolean allowOtherRecordTypeValues);

	/**
	 * Create a {@link RecordDecider} based on a single <i>record-type</i> field with a <i>default record</i> type.
	 * 
	 *  <p>You might use it for situations like:
	 *  
	 * <pre>
	 *       01  Header.
	 *           03 Record-Type            Pic x.
	 *              88 Header-Rec  value 'H'.
	 *              88 Trailer-Rec value 'T'.
	 *           ....
	 *       01  Detail.
	 *           03 Field-1                Pic x.
	 *           ....
	 *       01  Trailer.
	 *           03 Record-TypeT           Pic x.
	 *           ....
	 * </pre>
	 *
	 * The Java code
	 * 
	 *  <pre>
	 *     RecordDecider decider = JRecordInterface1.RECORD_DECIDER_BUILDER
	 *                                    .singleFieldDeciderBuilder("Record-Type", "Detail")
	 *                                        .addRecord("H", "Header")
	 *                                        .addRecord("T", "Trailer")
	 *                                    .build();
	 *     AbstractLineReader r = JRecordInterface1.COBOL
	 *              .newIOBuilder("file-name.cbl")
	 *                  .setRecordDecider(decider)
	 *                  ...                     
	 *  </pre>
	 *   
	 * @param recordTypeFieldName field name of the record type field
	 * @param defaultRecordName default record to use when the RecordType does not
	 * match any of the supplied Record-Types
	 * 
	 * @return Return request RecordDecider Builder.
	 */
	ISingleFieldDeciderBuilder singleFieldDeciderBuilder(String recordTypeFieldName, String defaultRecordName);

}