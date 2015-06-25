package eu.quanticol.carma.core.generator.carmavariable;

import eu.quanticol.carma.core.carma.VariableReference;
import eu.quanticol.carma.core.typing.BaseType;

public class VariableReferenceTypeSingleton {
	
	private static VariableReferenceTypeSingleton singleton;
	public static VariableReferenceTypeUtil util;
	
	private VariableReferenceTypeSingleton(){
		util = new VariableReferenceTypeUtil();
	}
	
	/* Static 'instance' method */
	public static VariableReferenceTypeSingleton getInstance() {
		if(singleton == null){
			singleton = new VariableReferenceTypeSingleton();
		} 
		return singleton;
	}
	
	public void setTool(VariableReferenceTool vrt){
		VariableReferenceTypeSingleton.util.setTool(vrt);
	}
	
	public BaseType type(String fqn, VariableReference vr){
		return VariableReferenceTypeSingleton.util.getType(fqn,vr);
	}

	public boolean isInitialised() {
		return VariableReferenceTypeSingleton.util.isInitialised();
	}
	
	public void initialised(){
		VariableReferenceTypeSingleton.util.initialised();
	}

}
