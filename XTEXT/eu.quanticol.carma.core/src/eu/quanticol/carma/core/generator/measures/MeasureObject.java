package eu.quanticol.carma.core.generator.measures;

import java.util.ArrayList;
import java.util.HashMap;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.EnvironmentExpressions;
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions;
import eu.quanticol.carma.core.carma.EnvironmentMeasure;
import eu.quanticol.carma.core.carma.Measure;
import eu.quanticol.carma.core.generator.Args;

public class MeasureObject {
	
	private String name;
	private String measureName;
	private String environmentMeasureName;
	private HashMap<String,ArrayList<String>> args;
	private ArrayList<ArrayList<String>> product;
	private EnvironmentMeasure em;

	public MeasureObject(String m, String e, HashMap<String,ArrayList<String>> args, EnvironmentExpressions em) {
		this.name = m + "_" + e;
		this.measureName = m;
		this.environmentMeasureName = e;
		this.args = args;
		this.em = (EnvironmentMeasure) em;
	}

	public String getName() {
		return name;
	}
	
	public EnvironmentMacroExpressions getEME(){
		return em.getComponentReference();
	}
	
	public BooleanExpressions getBES(){
		return em.getBooleanExpression();
	}
	
	public String getInArgs(){
		ArrayList<String> temp = new ArrayList<String>();
		for(String key : args.keySet()){
			temp.add("int " + key);
		}
		String output = temp.get(0);
		for(int i = 1; i < temp.size(); i++){
			output = output + ", " + temp.get(1);
		}
		return output;
	}
	
	public String getOutArgs(){
		ArrayList<String> temp = new ArrayList<String>();
		for(String key : args.keySet()){
			temp.add(key);
		}
		String output = temp.get(0);
		for(int i = 1; i < temp.size(); i++){
			output = output + ", " + temp.get(1);
		}
		return output;
	}
	
	public ArrayList<ArrayList<String>> produce(){
		this.product = hashToArray();
		ArrayList<ArrayList<String>> exploded = new ArrayList<ArrayList<String>>();
		cartesianProduct(this.product, exploded);
		this.product = exploded;
		return this.product;
	}
	
	public ArrayList<ArrayList<String>> hashToArray(){
		
		ArrayList<ArrayList<String>> temp = new ArrayList<ArrayList<String>>();
		
		for(String key : this.args.keySet()){
			temp.add(this.args.get(key));
		}
		
		return temp;
		
	}
	
	public void cartesianProduct(ArrayList<ArrayList<String>> array1, ArrayList<ArrayList<String>> array2){
		if(array1.size() > 1){
			ArrayList<String> head = array1.remove(0);
			ArrayList<ArrayList<String>> exit = new ArrayList<ArrayList<String>>();
			cartesianProduct(array1,array2);
			for(int i = 0; i < array2.size(); i++){
				for(String item : head){
					ArrayList<String> inter = new ArrayList<String>();
					inter.add(item);
					inter.addAll(array2.get(i));
					exit.add(inter);
				}
			}
			array2.clear();
			array2.addAll(exit);
		} else {
			ArrayList<String> head = array1.remove(0);
			for(String item : head){
				ArrayList<String> tail = new ArrayList<String>();
				tail.add(item);
				array2.add(tail);
			}
		}
	}

}
