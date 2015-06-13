package eu.quanticol.carma.core.generator.components;

import java.util.HashMap;

import eu.quanticol.carma.core.generator.actions.ActionManager;
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager;
import eu.quanticol.carma.core.utils.Tree;

public class ComponentManager {
	
	private HashMap<String,ComponentGenerator> components;
	private ActionManager am;
	private CarmaVariableManager vm;

	public ComponentManager(ActionManager am, CarmaVariableManager vm) {
		this.components = new HashMap<String,ComponentGenerator>();
		this.am = am;
		this.vm = vm;
	}
	
	public CarmaVariableManager getCVM(){
		return this.vm;
	}
	
	public ActionManager getAM(){
		return this.am;
	}
	
	public void loadComponent(String component_name, Tree tree, ActionManager am, CarmaVariableManager vm){
		ComponentGenerator cg = new ComponentGenerator(component_name, tree, am, vm);
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

}
