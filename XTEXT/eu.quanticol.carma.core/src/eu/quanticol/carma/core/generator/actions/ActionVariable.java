package eu.quanticol.carma.core.generator.actions;

import java.util.HashMap;
import java.util.HashSet;

import org.eclipse.xtext.xbase.lib.Extension;

import com.google.inject.Inject;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.EnvironmentOperation;
import eu.quanticol.carma.core.carma.EnvironmentUpdate;
import eu.quanticol.carma.core.carma.InputActionArguments;
import eu.quanticol.carma.core.carma.OutputActionArguments;
import eu.quanticol.carma.core.carma.Probability;
import eu.quanticol.carma.core.carma.Rate;
import eu.quanticol.carma.core.carma.Update;
import eu.quanticol.carma.core.generator.ExpressionHandler;

public class ActionVariable {
	
	@Inject
	@Extension
	private ExpressionHandler expressionHandler;
	
	private String carma_name = "";
	private int counter = 0;
	private HashMap<String,Integer> names;
	private HashMap<Integer,String> hashes;
	private HashMap<Integer,BooleanExpressions> predicates;
	private HashMap<Integer,Update> updates;
	private HashMap<Integer, OutputActionArguments> outputArgs;
	private HashMap<Integer, InputActionArguments> inputArgs;
	private String psf = "public static final ";
	private String eq = " = ";
	private String end = ";";
	private HashMap<String,String> rates;
	private HashMap<String,String> probs;
	private HashMap<String,String> updas;
	private boolean isBroadcast = false;
	private String modelName;
	
	
	public ActionVariable(String name, int counter, int hashCode, String modelName){
		this.counter = counter;
		this.modelName = modelName;
		setCarmaName(name);
		names = new HashMap<String, Integer>();
		hashes = new HashMap<Integer, String>();
		rates = new HashMap<String, String>();
		probs = new HashMap<String, String>();
		updas = new HashMap<String, String>();
		names.put(name,hashCode);
		hashes.put(hashCode, name);
		predicates = new HashMap<Integer, BooleanExpressions>();
		updates = new HashMap<Integer, Update>();
		outputArgs = new HashMap<Integer, OutputActionArguments>();
		inputArgs = new HashMap<Integer, InputActionArguments>();
		setBroadcast(name.contains("*"));
	}
	
	private void setCarmaName(String name){
		String friendly_name = name.replace("*", "");
		friendly_name = friendly_name.replace("<>","");
		friendly_name = friendly_name.replace("()","");
		carma_name = friendly_name.toUpperCase();
	}
	
	public void setUpdates(int hashCode, Update u){
		this.updates.put(hashCode, u);
	}
	
	public Update getUpdate(int hashCode){
		return this.updates.get(hashCode);
	}
	
	public void setPredicate(int hashCode, BooleanExpressions bes){
		this.predicates.put(hashCode, bes);
	}
	
	public BooleanExpressions getPredicate(int hashCode){
		return this.predicates.get(hashCode);
	}
	
	public String getStaticName(){
		return this.modelName+"Definition."+this.carma_name;
	}
	
	public String getCarmaName(){
		return this.carma_name;
	}
	
	public String declareAction(){
		return psf + "int " + carma_name + " " + eq + counter + end + "\n";
	}

	public void include(String name, int hashCode) {
		this.names.put(name, hashCode);
	}
	
	public String declareRates(){
		String output = "";
		for(String key : rates.keySet()){
			output = output + psf + "double " + carma_name + "_" + key + "_RATE " + eq + rates.get(key) + end + "\n";
		}
		
		return output;
	}
	
	public void setGuardExpression(String type, String guard, String expression){
		
		if(type.equals("rate")){
			rates.put(guard, expression);
		} 
		
		if(type.equals("prob")){
			probs.put(guard, expression);
		}
		
		if(type.equals("upda")){
			updas.put(guard, expression);
		}
	}

	public boolean isBroadcast() {
		return isBroadcast;
	}

	public void setBroadcast(boolean isBroadcast) {
		this.isBroadcast = isBroadcast;
	}

	public void setOutputActionArguments(int hashCode,
			OutputActionArguments outputArgs) {
		this.outputArgs.put(hashCode, outputArgs);
	}
	
	public OutputActionArguments getOutputActionArguments(int hashCode){
		return this.outputArgs.get(hashCode);
	}
	
	public void setInputActionArguments(int hashCode,
			InputActionArguments inputArgs) {
		this.inputArgs.put(hashCode, inputArgs);
	}
	
	public InputActionArguments getInputActionArguments(int hashCode){
		return this.inputArgs.get(hashCode);
	}

	public void loadModelName(String string) {
		this.modelName = string;
		
	}

}
