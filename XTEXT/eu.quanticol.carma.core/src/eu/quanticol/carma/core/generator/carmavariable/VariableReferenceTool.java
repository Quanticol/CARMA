package eu.quanticol.carma.core.generator.carmavariable;


import eu.quanticol.carma.core.carma.Name;
import eu.quanticol.carma.core.typing.BaseType;

public class VariableReferenceTool {
	
	private int code;
	private Name decName;
	private BaseType type;
	private String fqn;
	private Double assignment;
	
	public VariableReferenceTool(){
		this.type = new BaseType();
		this.decName = null;
		this.fqn = "null";
		this.assignment = 0.0;
		this.code = 0;
	}

	public int getCode() {
		return code;
	}

	public Name getDecName() {
		return decName;
	}

	public BaseType getType() {
		return type;
	}

	public String getFqn() {
		return fqn;
	}

	public Double getAssignment() {
		return assignment;
	}

	public void add(String fqn, Name decName, BaseType type) {
	  this.fqn = fqn;
	  this.decName = decName;
	  this.type = type;
	}

}
