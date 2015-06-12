package eu.quanticol.carma.core.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import eu.quanticol.carma.core.carma.Action;
import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.Guard;
import eu.quanticol.carma.core.carma.Process;
import eu.quanticol.carma.core.carma.ProcessExpression;
import eu.quanticol.carma.core.carma.ProcessExpressionChoice;

public class Tree {
	
	private HashMap<Process,Leaf> structure;
	
	public Tree(HashSet<Process> processes){
		structure = new HashMap<Process,Leaf>();
		for(Process process : processes){
			structure.put(process, new Leaf(process));
		}
	}
	
	public void getActions(HashMap<String,Action> actions){
		for(Process key : structure.keySet())
			this.structure.get(key).getAllActions(actions);
	}
	
	public void getGuards(HashMap<String,BooleanExpressions> guards){
		for(Process key : structure.keySet())
			this.structure.get(key).getAllGuards(guards);
	}
	
	public void getTransitions(ArrayList<String> transitions){
		for(Process key : structure.keySet())
			this.structure.get(key).getAllTransitions(transitions);
	}
	
	public void getStates(HashSet<String> states){
		for(Process key : structure.keySet())
			this.structure.get(key).getAllStates(states);
	}

}
