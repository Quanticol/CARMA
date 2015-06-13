package eu.quanticol.carma.core.generator.components;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import eu.quanticol.carma.core.carma.Action;
import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.Guard;
import eu.quanticol.carma.core.generator.actions.ActionManager;
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager;
import eu.quanticol.carma.core.utils.Tree;

public class ComponentGenerator {
	
	private String component_name = "";
	private Tree tree = null;
	private ActionManager am;
	private CarmaVariableManager vm;
	
	public ComponentGenerator(String component_name, Tree tree, ActionManager am, CarmaVariableManager vm){
		this.component_name = component_name;
		this.tree = tree;
		this.am = am;
		this.vm = vm;
	}
	
	public String declareCarmaProcessAutomaton(){
		String output = "";
		output = "public static final CarmaProcessAutomaton "+this.component_name+"Process = create"+this.component_name+"Process();" + "\n";
		return output;
	}
	
	public Tree getTree(){
		return this.tree;
	}
	
	public ArrayList<String> declareStates(){
		HashSet<String> states = new HashSet<String>();
		tree.getStates(states);
		ArrayList<String> output = new ArrayList<String>();
		for(String state : states){
			if(!state.equals("null"))
				output.add("CarmaProcessAutomaton.State "+state+" = toReturn.newState("+'"'+state+'"'+");");
		}
		return output;
	}
	
	public ArrayList<String> declareTransitions(){
		ArrayList<String> transitions = new ArrayList<String>();
		tree.getTransitions(transitions);
		ArrayList<String> output = new ArrayList<String>();
		for(String transition : transitions){
			output.add(transition);
		}
		return output;
	}
	
	public HashMap<String,BooleanExpressions> declareGuards(){
		HashMap<String,BooleanExpressions> output = new HashMap<String,BooleanExpressions>();
		tree.getGuards(output);
		return output;
	}
	
	public HashMap<String,Action> declareActions(){
		HashMap<String,Action> actions = new HashMap<String,Action>();
		tree.getActions(actions);
		return actions;
	}


}
