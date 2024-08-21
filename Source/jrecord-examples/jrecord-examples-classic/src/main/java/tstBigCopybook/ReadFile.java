package tstBigCopybook;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.w3c.dom.Document;

import net.sf.cb2xml.Cb2Xml2;

public class ReadFile {

	private JFrame frame = new JFrame();
	private JTextField txt = new JTextField(40);
	private JButton go = new JButton("go");
	
	Document convertToXMLDOM;
	
//	private final String fname;
	
	ReadFile(String fname) {
		
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		
		JPanel pnl =new JPanel(new FlowLayout(FlowLayout.LEFT));
		txt.setText(fname);
		pnl.add(txt);
		pnl.add(go);
		frame.getContentPane().add(pnl);
		frame.pack();
		frame.setVisible(true);
		go.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				doRead();
			}
		});	
	}
	
	private void doRead()  {
		String fname = txt.getText();
		
		try {
			convertToXMLDOM = Cb2Xml2.convertToXMLDOM(new File(fname));
			txt.setText("Done");
		} catch (Exception e) {
			txt.setText("failed");
			e.printStackTrace();
		}
	}
	public static void main(String[] args) {
		new ReadFile(args == null || args.length == 0 ? "LargeCopybook.cbl" : args[0]);
	}

}
