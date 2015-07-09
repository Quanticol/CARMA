package eu.quanticol.carma.core.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import eu.quanticol.carma.core.carma.Action;
import eu.quanticol.carma.core.carma.ActionName;
import eu.quanticol.carma.core.carma.BooleanExpression;
import eu.quanticol.carma.core.carma.Process;
import eu.quanticol.carma.core.carma.ProcessExpression;
import eu.quanticol.carma.core.carma.ProcessExpressionAction;
import eu.quanticol.carma.core.carma.ProcessExpressionChoice;
import eu.quanticol.carma.core.carma.ProcessExpressionGuard;
import eu.quanticol.carma.core.carma.ProcessExpressionLeaf;
import eu.quanticol.carma.core.carma.ProcessExpressionReference;
import eu.quanticol.carma.core.carma.ProcessName;


public class Leaf {
	
	private Process process;
	private Leaf parent;
	private ProcessExpression me;
	//state to Leaf
	private HashMap<String,Leaf> children;
	private String state;
	private BooleanExpression guard = null;
	private Action action = null;
	private boolean choice = false;
	private boolean leaf = false;
	private boolean reference = false;
	
	public Leaf(Process p){
		this.process = p;
		this.me = p.getProcessExpression();
		this.state = ((ProcessName)p.getName()).getName();
		this.parent = null;
		this.children = new HashMap<String,Leaf>();
		this.setChildren();
	}
	
	public Leaf(Process p, ProcessExpression pe, Leaf parent){
		this.process = p;
		this.me = pe;
		this.state = getState(me);
		this.parent = parent;
		this.children = new HashMap<String,Leaf>();
		this.setChildren();
	}
	
	public Leaf(Process p, ProcessExpression pe, Leaf parent, String state){
		this.process = p;
		this.me = pe;
		this.state = state;
		this.parent = parent;
		this.children = new HashMap<String,Leaf>();
		this.setChildren();
	}
	
	public String getAction(){
		if(this.action != null){
			return "action_"+((ActionName) this.action.getName()).getName();
		}
		return "";
	}
	
	public String getGuard(){
		if(this.guard != null){
			return "guard_"+this.state;
		}
		return "";
	}
	
	public String getState(){
		if(this.state.equals("null"))
			return "null";
		return "state_"+this.state;
	}
	
	public ArrayList<String> getTransitions(){
		ArrayList<String> output = new ArrayList<String>();
		if(children != null)
			for(String key : children.keySet()){
				if(this.guard == null){
					if(this.action != null){
						output.add("toReturn.addTransition("+this.getState()+","+getAction()+","+children.get(key).getState()+");");
					}
				} else {
					if(this.action != null){
						output.add("toReturn.addTransition("+this.getState()+","+getGuard()+","+getAction()+","+children.get(key).getState()+");");
					} else {
						output.add("toReturn.addTransition("+this.getState()+","+getGuard()+","+"NOFOLLOWINGACTION!,"+children.get(key).getState()+");");
					}
					
				}
			}
		return output;
	}
	
	public void getAllActions(HashMap<String,Action> actions){
		if(this.action != null)
			actions.put(getAction(),this.action);
		if(children != null)
			for(String key : children.keySet()){
				children.get(key).getAllActions(actions);
			}
	}
	
	public void getAllGuards(HashMap<String,BooleanExpression> guards){
		if(this.guard != null)
			guards.put(getGuard(), this.guard);
		if(children != null)
			for(String key : children.keySet()){
				children.get(key).getAllGuards(guards);
			}
	}
	
	public void getAllTransitions(ArrayList<String> transitions){
		transitions.addAll(this.getTransitions());
		if(children != null)
			for(String key : children.keySet()){
				children.get(key).getAllTransitions(transitions);
			}
	}
	
	public void getAllStates(HashSet<String> states){
		states.add(getState());
		if(children != null)
			for(String key : children.keySet()){
				children.get(key).getAllStates(states);
			}
	}
	
	public String getState(ProcessExpression pe){
		String state = "";
		boolean matched = false;
		if(!matched){
			if (pe instanceof ProcessExpressionChoice) {
				matched=true;
		        ProcessExpression left = ((ProcessExpressionChoice)pe).getLeft();
		        ProcessExpression right = ((ProcessExpressionChoice)pe).getRight();
		        state = state + "Choice_" + this.getState(left) + "_" + this.getState(right);
			}
		}
		if(!matched){
			if (pe instanceof ProcessExpressionLeaf) {
				matched=true;
				ProcessExpression expression = ((ProcessExpressionLeaf)pe);
				state = "null";
			}
		}
		if(!matched){
			if (pe instanceof ProcessExpressionGuard) {
				matched=true;
				ProcessExpression reference = ((ProcessExpressionGuard)pe).getReference();
				state = state + this.getState(reference);
			}
		}
		if(!matched){
			if (pe instanceof ProcessExpressionAction) {
				matched=true;
				Action expression = ((ProcessExpressionAction)pe).getExpression();
				String name = ((ActionName) expression.getName()).getName();
				ProcessExpression reference = ((ProcessExpressionAction)pe).getReference();
				state = state + name +this.getState(reference);
			}
		}
		if(!matched){
			if (pe instanceof ProcessExpressionReference) {
				matched=true;
				ProcessName reference = ((ProcessExpressionReference)pe).getExpression();
				state = state + reference.getName();
			}
		}
		return state;
	}
	
	public void setChildren(){
		boolean matched = false;
		if(!matched){
			if (this.me instanceof ProcessExpressionChoice) {
//				System.out.println("choice");
				matched=true;
		        ProcessExpression left = ((ProcessExpressionChoice)this.me).getLeft();
		        ProcessExpression right = ((ProcessExpressionChoice)this.me).getRight();
		        this.children.put(this.getState()+"_left", new Leaf(this.process,left,this,this.state));
		        this.children.put(this.getState()+"_right", new Leaf(this.process,right,this,this.state));
		        this.choice = false;
			}
		}
		if(!matched){
			if (this.me instanceof ProcessExpressionLeaf) {
//				System.out.println("leaf");
				matched=true;
				this.children = null;
				this.leaf = true;
			}
		}
		if(!matched){
			if (this.me instanceof ProcessExpressionGuard) {
//				System.out.println("guard");
				matched=true;
				this.guard						= ((ProcessExpressionGuard)this.me).getExpression().getBooleanExpression();
				ProcessExpression reference		= ((ProcessExpressionGuard)this.me).getReference();
				if(reference instanceof ProcessExpressionAction){
					this.action 	= ((ProcessExpressionAction)reference).getExpression();
					reference		= ((ProcessExpressionAction)reference).getReference();
				}
				this.children.put(this.state,new Leaf(this.process,reference,this));
			}
		}
		if(!matched){
			if (this.me instanceof ProcessExpressionAction) {
//				System.out.println("action");
				matched=true;
				this.action						= ((ProcessExpressionAction)this.me).getExpression();
				ProcessExpression reference		= ((ProcessExpressionAction)this.me).getReference();
				this.children.put(this.state,new Leaf(this.process,reference,this));
			}
		}
		if(!matched){
			if (this.me instanceof ProcessExpressionReference) {
//				System.out.println("reference");
				matched=true;
				this.children = null;
				this.reference = true;
			}
		}
	}

}