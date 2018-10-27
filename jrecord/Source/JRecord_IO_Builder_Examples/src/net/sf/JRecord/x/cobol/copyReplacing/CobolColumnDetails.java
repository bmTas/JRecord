package net.sf.JRecord.x.cobol.copyReplacing;


public class CobolColumnDetails {
	private static final int LONG_LINE_LENGTH = 16000;
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;

//	private static int[] END_COLS = new int[Cb2xmlConstants.FREE_FORMAT];
//	
//	static {
//		END_COLS[Cb2xmlConstants.USE_STANDARD_COLUMNS] = LAST_COBOL_COLUMN;
//		END_COLS[Cb2xmlConstants.USE_COLS_6_TO_80] =  80;
//		END_COLS[Cb2xmlConstants.USE_LONG_LINE] = LONG_LINE_LENGTH;
//	}

	public static final CobolColumnDetails
			STANDARD_COLUMNS  = new CobolColumnDetails(FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN),
			FREE_FORMAT       = new CobolColumnDetails(0, Integer.MAX_VALUE),
			LONG_LINE         = new CobolColumnDetails(FIRST_COBOL_COLUMN, LONG_LINE_LENGTH),
			COLUMNS_6_80      = new CobolColumnDetails(FIRST_COBOL_COLUMN, 80);
	private final int startColumn, endColumn;

	public CobolColumnDetails(int startColumn, int endColumn) {
		super();
		this.startColumn = startColumn;
		this.endColumn = endColumn;
	}
	

	public int getStartColumn() {
		return startColumn;
	}

	public int getEndColumn() {
		return endColumn;
	}

}
