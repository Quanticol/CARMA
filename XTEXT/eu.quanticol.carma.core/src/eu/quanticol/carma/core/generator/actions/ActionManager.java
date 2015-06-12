package eu.quanticol.carma.core.generator.actions;

import java.util.HashMap;

import eu.quanticol.carma.core.carma.EnvironmentOperation;

public class ActionManager {
	
	private HashMap<String,ActionVariable> actions;
	private int counter = 0;
	
	public ActionManager(){
		this.actions = new HashMap<String,ActionVariable>();
	}
	
	public void loadAction(String actionName){
		ActionVariable av = new ActionVariable(actionName,counter);
		String name = av.getCarmaName();
		if(actions.containsKey(name)){
			actions.get(name).include(actionName);
		} else {
			actions.put(name, av);
		}
		counter++;
	}
	
	public void loadStub(String actionName, String type, String guard, String expression){
		if(actions.containsKey(actionName.toUpperCase()))
			actions.get(actionName.toUpperCase()).setGuardExpression(type,guard,expression);
	}
	
	public String declareAllActionsAndRates(){
		String output = "";
		for(String key : actions.keySet()){
			output = output + actions.get(key).declareAction();
			output = output + actions.get(key).declareRates();
		}
		return output;
	}
	
	public ActionVariable get(String key){
		String friendly_name = key.replace("*", "");
		friendly_name = friendly_name.replace("<>","");
		friendly_name = friendly_name.replace("()","");
		return this.actions.get(friendly_name.toUpperCase());
	}

}
