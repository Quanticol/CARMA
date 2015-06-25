package eu.quanticol.carma.core.generator.measures;

import eu.quanticol.carma.core.carma.BooleanExpressions;
import eu.quanticol.carma.core.carma.EnvironmentExpressions;
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions;
import eu.quanticol.carma.core.carma.SetComp;

public class EnvironmentMeasureObject {

	private String name;
	private SetComp em;
	
	public EnvironmentMeasureObject(String name, EnvironmentExpressions em) {
		this.name = name;
		this.em = (SetComp) em;
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
