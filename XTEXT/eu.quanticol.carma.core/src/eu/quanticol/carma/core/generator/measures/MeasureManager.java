package eu.quanticol.carma.core.generator.measures;

import java.util.ArrayList;
import java.util.HashMap;

import eu.quanticol.carma.core.generator.actions.ActionManager;
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager;
import eu.quanticol.carma.core.generator.components.ComponentManager;
import eu.quanticol.carma.core.carma.EnvironmentExpressions;
import eu.quanticol.carma.core.carma.Measure;
import eu.quanticol.carma.core.carma.EnvironmentMeasure;

public class MeasureManager {

	private ActionManager am;
	private CarmaVariableManager vm;
	private ComponentManager cm;
	private HashMap<String,MeasureObject> measures;
	private HashMap<String,EnvironmentMeasureObject> environmentMeasures;
	private HashMap<String,ArrayList<String>> systemMeasure;
	
	public MeasureManager(ActionManager am, CarmaVariableManager vm, ComponentManager cm) {
		this.am = am;
		this.vm = vm;
		this.cm = cm;
		this.measures = new HashMap<String, MeasureObject>();
		this.environmentMeasures = new HashMap<String, EnvironmentMeasureObject>();
		this.systemMeasure = new HashMap<String, ArrayList<String>>();
	}

	public void loadMeasure(String m, String e, HashMap<String,ArrayList<String>> args, EnvironmentExpressions em) {
	  MeasureObject temp = new MeasureObject(m, e, args, em);
	  this.measures.put(temp.getName(), temp);
	}

	public void loadEnvMeasure(String name, EnvironmentExpressions em) {
	  EnvironmentMeasureObject temp = new EnvironmentMeasureObject(name,em);
	  this.environmentMeasures.put(temp.getName(),temp);
	}
	
	public HashMap<String,MeasureObject> getMeasures(){
		return this.measures;	
	}
	
	public HashMap<String,EnvironmentMeasureObject> getEnvMeasures(){
		return this.environmentMeasures;	
	}
	
	public CarmaVariableManager getCVM(){
		return vm;
	}

	public void loadSystemMeasure(String system, String measure) {
		if(this.systemMeasure.containsKey(system)){
			this.systemMeasure.get(system).add(measure);
		} else{
			ArrayList<String> measures = new ArrayList<String>();
			measures.add(measure);
			this.systemMeasure.put(system,measures);
		}
	}
	
	public ArrayList<String> getMeasures(String systemName){
		if(this.systemMeasure.size() > 0)
			return this.systemMeasure.get(systemName);
		else
			return new ArrayList<String>();
	}

}
