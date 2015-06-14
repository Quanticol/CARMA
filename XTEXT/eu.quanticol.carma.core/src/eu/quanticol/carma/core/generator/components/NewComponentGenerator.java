package eu.quanticol.carma.core.generator.components;

import java.util.ArrayList;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.ForVariableDeclaration;
import eu.quanticol.carma.core.carma.MethodExpressions;
import eu.quanticol.carma.core.carma.VariableReference;

public class NewComponentGenerator {
	
	private String name;
	private ArrayList<ArrayList<String>> arguments;
	private ArrayList<ArrayList<String>> product;
	private boolean isFor;
	private ForVariableDeclaration fvd;
	private BooleanExpressions bes;
	private VariableReference vr;
	private MethodExpressions mes;

	public NewComponentGenerator(String name,
			ArrayList<ArrayList<String>> arguments, 
			boolean isFor,
			ForVariableDeclaration fvd,
			BooleanExpressions bes, 
			VariableReference vr, 
			MethodExpressions mes) {
		this.name = name;
		this.arguments = arguments;
		this.isFor = isFor;
	}

	public String getname() {
		return name;
	}
	
	public ArrayList<ArrayList<String>> produce(){
		this.product = new ArrayList<ArrayList<String>>();
		cartesianProduct(this.arguments, this.product);
		return this.product;
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
	
	public boolean isFor(){
		return this.isFor;
	}

	public MethodExpressions getMes() {
		return mes;
	}

	public VariableReference getVr() {
		return vr;
	}

	public BooleanExpressions getBes() {
		return bes;
	}

	public ForVariableDeclaration getFvd() {
		return fvd;
	}

	public ArrayList<String> getArguments(){
		ArrayList<String> output = new ArrayList<String>();
		
		for(ArrayList<String> list : this.arguments){
			for(String s : list){
				output.add(s);
			}
		}
		return output;
	}


}
