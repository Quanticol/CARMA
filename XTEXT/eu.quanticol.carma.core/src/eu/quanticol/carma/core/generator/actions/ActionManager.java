package eu.quanticol.carma.core.generator.actions;

import java.util.HashMap;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.EnvironmentOperation;
import eu.quanticol.carma.core.carma.InputActionArguments;
import eu.quanticol.carma.core.carma.OutputActionArguments;
import eu.quanticol.carma.core.carma.Update;

public class ActionManager {
	
	private HashMap<String,ActionVariable> actions;
	private int counter = 0;
	
	public ActionManager(){
		this.actions = new HashMap<String,ActionVariable>();
	}
	
	public void loadAction(String actionName, int hashCode, String modelName){
		ActionVariable av = new ActionVariable(actionName,counter,hashCode,modelName);
		String name = av.getCarmaName();
		if(actions.containsKey(name)){
			actions.get(name).include(actionName,hashCode);
		} else {
			actions.put(name, av);
		}
		counter++;
	}
	
	public void loadPredicate(String actionName, int hashCode, BooleanExpressions bes){
		if(actions.containsKey(actionName.toUpperCase()))
			actions.get(actionName.toUpperCase()).setPredicate(hashCode, bes);
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

	public void loadUpdate(String actionName, int hashCode, Update update) {
		if(actions.containsKey(actionName.toUpperCase()))
			actions.get(actionName.toUpperCase()).setUpdates(hashCode, update);
	}
	
	public void loadOutputActionArguments(String actionName, int hashCode, OutputActionArguments outputArgs) {
		if(actions.containsKey(actionName.toUpperCase()))
			actions.get(actionName.toUpperCase()).setOutputActionArguments(hashCode, outputArgs);
	}
	
	public void loadInputActionArguments(String actionName, int hashCode, InputActionArguments inputArgs) {
		if(actions.containsKey(actionName.toUpperCase()))
			actions.get(actionName.toUpperCase()).setInputActionArguments(hashCode, inputArgs);
	}

}
