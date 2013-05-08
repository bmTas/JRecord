package net.sf.JRecord.Common;

import java.util.Properties;

public class UserInit {

	static {

		Properties properties = PropertyManager.getProperties();
		String init = "init.";

		String var, className;
		Object o;
		@SuppressWarnings("rawtypes")
		Class c;

		if (properties != null) {
			for (int i = 0; i < 32; i++) {
				var = init + i;
				if (properties.containsKey(var)) {
					try {
						className = properties.getProperty(var);
						c = Class.forName(className);
						if (c != null) {
							o = c.newInstance();

							if (o instanceof Runnable) {
								((Runnable) o).run();
							}
						}
					} catch (Exception e) {
						// TODO: handle exception
					}
				}
			}
		}
	}

	public static void init() {

	}
}
