package eu.quanticol.carma.core.generator.carmaVariable;

import java.util.HashMap;

import eu.quanticol.carma.core.carma.VariableReference;

public class CarmaVariableManager {
	
	private HashMap<String,CarmaVariable> variables;
	private HashMap<String,CarmaVariable> types;
	private HashMap<String,CarmaVariable> references;
	private HashMap<String,String> prefixes;
	
	public CarmaVariableManager(){
		variables  = new HashMap<String,CarmaVariable>();
		types  = new HashMap<String,CarmaVariable>();
		references = new HashMap<String,CarmaVariable>();
		prefixes = new HashMap<String, String>();
	}
	
	public void loadDeclaration(String name, Boolean isInt, Boolean isRecord, int hashCode, Object value){
		CarmaVariable cv = new CarmaVariable(name,isInt,isRecord);
		cv.addValue(hashCode,value);
		if(variables.containsKey(cv.getName())){
			variables.get(cv.getName()).addValue(hashCode,value);
		} else {
			variables.put(cv.getName(), cv);
		}
	}
	
	public void loadType(String name, Boolean isInt, Boolean isRecord, int hashCode, Object value){
		CarmaVariable cv = new CarmaVariable(name,isInt,isRecord);
		cv.addValue(hashCode,value);
		if(types.containsKey(cv.getName())){
			types.get(cv.getName()).addValue(hashCode,value);
		} else {
			types.put(cv.getName(), cv);
		}
	}
	
	public void loadReference(String name, Boolean isInt, Boolean isRecord, int hashCode, Object value, String prefix){
		CarmaVariable cv = new CarmaVariable(name,isInt,isRecord);
		cv.addValue(hashCode,value);
		cv.setPrefix(prefix);
		if(references.containsKey(cv.getName())){
			references.get(cv.getName()).addValue(hashCode,value);
		} else {
			references.put(cv.getName(), cv);
		}
		prefixes.put(prefix.toUpperCase()+cv.getName(),cv.getName());
	}
	
	public String declareAllAttributesAndTypes(){
		String output = "";
		for(String key : variables.keySet()){
			output = output + variables.get(key).declareCarma() + "\n";
			output = output + variables.get(key).declareCarmaType() + "\n";
		}
		return output;
	}
	
	public String cleanName(String dirty){
		return dirty.replace(".", "_").toUpperCase() + "_ATTRIBUTE";
	}
	public String getCarmaName(String name, VariableReference vr){
		return this.prefixes.get(name);
	}
	
	public String getJavaDeclaration(String name, VariableReference vr, String modifier){
		String cv_name = this.prefixes.get(name);
		return this.references.get(cv_name).declareJava(modifier);
	}
	
	public String getJavaAssign(String name, VariableReference vr, String modifier){
		String cv_name = this.prefixes.get(name);
		return this.references.get(cv_name).assignJava(modifier);
	}
	
	public boolean contains(String name) {
		return this.variables.containsKey(name);
	}

}
