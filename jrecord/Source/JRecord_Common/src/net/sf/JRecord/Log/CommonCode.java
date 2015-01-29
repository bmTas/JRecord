package net.sf.JRecord.Log;

import java.io.IOException;

public final class CommonCode {

	public static void append(Appendable buf, Exception ex) {
		
		StackTraceElement[] el = ex.getStackTrace();
		int fin = Math.min(1000, el.length);

		try {
			buf.append("\n\nClass=");
			buf.append(ex.getClass().getName());
			buf.append("    Error=");
			buf.append(ex.getMessage());
			buf.append("\n");

			for (int i = 0; i < fin; i++) {
				buf.append("\n    ");
				buf.append(el[i].getClassName());
				buf.append("  :  ");
				buf.append(Integer.toString(el[i].getLineNumber()));
			}
		} catch (IOException e) {
			printAppendError(e, ex);
		}

	}
	
	public static void printAppendError(Exception e, Exception ex) {
		System.err.println();
		System.err.println("Error in Appending: ");
		e.printStackTrace();
		if (ex != null) {
			System.err.println();
			System.err.println("Caused by: ");
			ex.printStackTrace();
		}

	}
}
