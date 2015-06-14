package eu.quanticol.carma.core.generator;

import java.util.ArrayList;
import java.util.HashMap;

public class Args {

	private HashMap<String,ArrayList<String>> args;
	
	public Args(){
		args = new HashMap<String, ArrayList<String>>();
	}
	
	public void add(String name, ArrayList<String> list){
		args.put(name,list);
	}


}
