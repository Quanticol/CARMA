package eu.quanticol.carma.core.generator.components;

import java.util.ArrayList;

public class Spawn {
	
	private String name;
	private ArrayList<ArrayList<String>> args;
	private int hashcode;
	private ArrayList<ArrayList<String>> explode;
	
	public Spawn(String name, ArrayList<ArrayList<String>> args, int hashcode){
		this.name = name;
		this.args = args;
		this.hashcode = hashcode;
		this.explode = new ArrayList<ArrayList<String>>();
	}
	
	public String getName(){
		return this.name;
	}
	
	public ArrayList<ArrayList<String>> produce(){
		cartesianProduct(this.args, this.explode);
		return this.explode;
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