package net.sf.JRecord.zTest.Common;

public class JUnit3Test {

	public void assertEqualsV3o(String msg, Object expected, Object actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual, msg);
	}
	
//	public void assertEquals(Object expected, Object actual) {
//		org.junit.jupiter.api.Assertions.assertEquals(expected, actual);
//	}

	public void assertEqualsV3i(String msg, int expected, int actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual, msg);
	}
//	public void assertEquals(int expected, int actual) {
//		org.junit.jupiter.api.Assertions.assertEquals(expected, actual);
//	}

	public void assertEqualsV3(String msg, long expected, long actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual, msg);
	}

	public void assertEqualsV3(String msg, byte expected, byte actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual, msg);
	}


	public void assertTrueV3(String msg, boolean ok) {
		org.junit.jupiter.api.Assertions.assertTrue(ok, msg);
	}

//	public void assertTrue(boolean ok) {
//		org.junit.jupiter.api.Assertions.assertTrue(ok);
//	}
	

	public void assertFalseV3(String msg, boolean ok) {
		org.junit.jupiter.api.Assertions.assertFalse(ok, msg);
	}

//	public void assertFalse(boolean ok) {
//		org.junit.jupiter.api.Assertions.assertFalse(ok);
//	}

}
