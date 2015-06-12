package eu.quanticol.carma.core.generator.actions;

import java.util.HashMap;
import java.util.HashSet;

import org.eclipse.xtext.xbase.lib.Extension;

import com.google.inject.Inject;

import eu.quanticol.carma.core.carma.EnvironmentOperation;
import eu.quanticol.carma.core.carma.EnvironmentUpdate;
import eu.quanticol.carma.core.carma.Probability;
import eu.quanticol.carma.core.carma.Rate;
import eu.quanticol.carma.core.generator.ExpressionHandler;

public class ActionVariable {
	
	@Inject
	@Extension
	private ExpressionHandler expressionHandler;
	
	private String carma_name = "";
	private int counter = 0;
	private HashSet<String> names;
	private String psf = "public static final ";
	private String eq = " = ";
	private String end = ";";
	private HashMap<String,String> rates;
	private HashMap<String,String> probs;
	private HashMap<String,String> updas;
	
	
	public ActionVariable(String name, int counter){
		this.counter = counter;
		setCarmaName(name);
		names = new HashSet<String>();
		rates = new HashMap<String, String>();
		probs = new HashMap<String, String>();
		updas = new HashMap<String, String>();
		names.add(name);
	}
	
	private void setCarmaName(String name){
		String friendly_name = name.replace("*", "");
		friendly_name = friendly_name.replace("<>","");
		friendly_name = friendly_name.replace("()","");
		carma_name = friendly_name.toUpperCase();
	}
	
	public String getCarmaName(){
		return this.carma_name;
	}
	
	public String declareAction(){
		return psf + "int " + carma_name + " " + eq + counter + end + "\n";
	}

	public void include(String name) {
		this.names.add(name);
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

}
