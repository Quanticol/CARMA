package eu.quanticol.carma.core.generator.components;

import java.util.HashMap;

import eu.quanticol.carma.core.carma.AttribVariableDeclaration;
import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.MacroExpressionReference;
import eu.quanticol.carma.core.carma.MethodExpressions;
import eu.quanticol.carma.core.carma.VariableReference;
import eu.quanticol.carma.core.generator.actions.ActionManager;
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager;
import eu.quanticol.carma.core.utils.Tree;

import java.util.ArrayList;

public class ComponentManager {
	
	private HashMap<String,ComponentGenerator> components;
	private ActionManager am;
	private CarmaVariableManager vm;
	private HashMap<String,NewComponentGenerator> newDecs;
	private HashMap<Integer,Spawn> spawns;

	public ComponentManager(ActionManager am, CarmaVariableManager vm) {
		this.components = new HashMap<String,ComponentGenerator>();
		this.newDecs = new HashMap<String,NewComponentGenerator>();
		this.am = am;
		this.vm = vm;
		this.spawns = new HashMap<Integer, Spawn>();
	}
	
	public CarmaVariableManager getCVM(){
		return this.vm;
	}
	
	public ActionManager getAM(){
		return this.am;
	}
	
	public void loadComponent(String component_name, Tree tree, ArrayList<String> args, ArrayList<MacroExpressionReference> macros){
		ComponentGenerator cg = new ComponentGenerator(component_name, tree, am, vm, args, macros);
		this.components.put(component_name,cg);
	}
	
	public String declareAllCarmaProcessAutomatons(){
		String output = "";
		for(String key : components.keySet()){
			output = output + components.get(key).declareCarmaProcessAutomaton();
		}
		return output;
	}
	
	public HashMap<String,ComponentGenerator> getComponentGenerators(){
		return components;
	}
	
	public Tree getTree(String name){
		return components.get(name).getTree();
	}

	public void loadNew(String name,
			ArrayList<ArrayList<String>> arguments, 
			boolean isFor,
			AttribVariableDeclaration fvd,
			BooleanExpressions bes, 
			VariableReference vr, 
			MethodExpressions mes) {
		
	  NewComponentGenerator temp = new NewComponentGenerator(name,arguments,isFor, fvd, bes, vr, mes);
	  this.newDecs.put(temp.getname(),temp);
	}
	
	public HashMap<String,NewComponentGenerator> getNewDecs(){
		return this.newDecs;
	}

	public void loadVDS(String string, HashMap<String, Integer> map) {
	  this.components.get(string).addVDS(map);
	}

	public void loadSpawn(String string, ArrayList<ArrayList<String>> lists, int i) {
		Spawn temp = new Spawn(string,lists,i);
		this.spawns.put(i,temp);
	}
	
	public HashMap<Integer,Spawn> getSpawns(){
		return this.spawns;
	}

}
