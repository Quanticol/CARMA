package eu.quanticol.carma.core.generator.carmavariable;

import java.util.HashMap;

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.core.carma.VariableReference;
import eu.quanticol.carma.core.typing.BaseType;


public class VariableReferenceTypeUtil {
	
	private HashMap<String, VariableReferenceTool> references;
	private boolean isInitialised = false;

	public VariableReferenceTypeUtil(){
		this.references = new HashMap<String, VariableReferenceTool>();
	}

	public BaseType getType(String fqn, VariableReference reference) {
		if(this.references.containsKey(fqn)){
			return this.references.get(reference).getType();
		}
		return new BaseType();
	}

	public void setTool(VariableReferenceTool vrt) {
		this.references.put(vrt.getFqn(),vrt);
	}

	public boolean isInitialised() {
		return isInitialised;
	}
	
	public void initialised(){
		this.isInitialised = true;
	}
}
