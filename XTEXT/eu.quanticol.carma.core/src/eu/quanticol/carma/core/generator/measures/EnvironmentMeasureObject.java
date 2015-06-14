package eu.quanticol.carma.core.generator.measures;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.EnvironmentExpressions;
import eu.quanticol.carma.core.carma.EnvironmentMeasure;
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions;

public class EnvironmentMeasureObject {

	private String name;
	private EnvironmentMeasure em;
	
	public EnvironmentMeasureObject(String name, EnvironmentExpressions em) {
		this.name = name;
		this.em = (EnvironmentMeasure) em;
	}
	
	public String getName(){
		return this.name;
	}

	public EnvironmentMacroExpressions getEME() {
	  return em.getComponentReference();
	}
	
	public BooleanExpressions getBES(){
		return em.getBooleanExpression();
	}

	public String getGetInArgs() {
	  return "";
	}

	public String getGetOutArgs() {
	  return "";
	}

}
