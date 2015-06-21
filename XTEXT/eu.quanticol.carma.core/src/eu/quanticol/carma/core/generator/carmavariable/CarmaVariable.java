package eu.quanticol.carma.core.generator.carmavariable;

import java.util.ArrayList;
import java.util.HashMap;

import eu.quanticol.carma.core.carma.CarmaInteger;
import eu.quanticol.carma.core.carma.DoubleAssignment;
import eu.quanticol.carma.core.carma.DoubleAssignmentCarmaDouble;
import eu.quanticol.carma.core.carma.DoubleAssignmentMethodReference;
import eu.quanticol.carma.core.carma.DoubleAssignmentVariableName;
import eu.quanticol.carma.core.carma.EnumAssignment;
import eu.quanticol.carma.core.carma.EnumAssignmentCarmaInteger;
import eu.quanticol.carma.core.carma.EnumAssignmentMethodReference;
import eu.quanticol.carma.core.carma.EnumAssignmentRange;
import eu.quanticol.carma.core.carma.EnumAssignmentVariableName;
import eu.quanticol.carma.core.carma.IntegerAssignment;
import eu.quanticol.carma.core.carma.IntegerAssignmentCarmaInteger;
import eu.quanticol.carma.core.carma.IntegerAssignmentMethodReference;
import eu.quanticol.carma.core.carma.IntegerAssignmentVariableName;
import eu.quanticol.carma.core.carma.MethodReferenceMethodDeclaration;
import eu.quanticol.carma.core.carma.PrimitiveType;
import eu.quanticol.carma.core.carma.Range;
import eu.quanticol.carma.core.carma.RecordDeclaration;
import eu.quanticol.carma.core.carma.RecordName;
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaDouble;
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaIntger;
import eu.quanticol.carma.core.carma.VariableDeclarationEnum;
import eu.quanticol.carma.core.carma.VariableName;


public class CarmaVariable {
	
	private String carma_attribute = "";
	private String carma_attribute_type = "";
	private String java_name = "";
	private String java_type = "int";
	private String class_dec = "Class<Integer>";
	private String class_ass = "Integer.class";
	private boolean isRecord = false;
	private boolean isInt = true;
	private boolean isGlobal = false;
	private HashMap<Integer,CarmaVariableAssignment> assigns;
	private String prefix;
	private String psf = "public static final ";
	private String eq = " = ";
	private String end = ";";
	private char q = '"';
	
	//assumes that if this is a record, we have variable_record
	public CarmaVariable(String name, Boolean isInt, Boolean isRecord, Boolean isGlobal){
		setAttribute(name);
		setType(name);
		setJavaName(name);
		if(!isInt)
			changeTypeToDouble();
		this.isRecord = isRecord;
		this.assigns = new HashMap<Integer,CarmaVariableAssignment>();
		this.isGlobal = isGlobal;
	}
	
	private void changeTypeToDouble(){
		class_dec = "Class<Double>";
		class_ass = "Double.class";
		java_type = "double";
	}
	
	private void setJavaName(String name){
		java_name = name.toLowerCase();
	}
	
	private void setAttribute(String name){
		carma_attribute = name.toUpperCase() + "_ATTRIBUTE";
	}
	
	private void setType(String name){
		carma_attribute_type = name.toUpperCase() + "_ATTRIBUTE_TYPE";
	}
	
	public String declareCarma(){
		return psf + "String " + this.carma_attribute + eq + q + this.carma_attribute+ q + end;
	}
	
	public String declareCarmaType(){
		return psf + this.class_dec + " " +this.carma_attribute_type + eq + this.class_ass + end;
	}
	
	public String getName(){
		return carma_attribute;
	}
	
	public String getJavaName(String modifier){
		if(modifier.length() > 0)
			return java_name + "_" + modifier;
		else
			return java_name;
	}
	
	public String getJavaType(){
		return java_type;
	}
	
	public String declareJava(String modifier){
		return java_type + " " + getJavaName(modifier);
	}
	
	public String assignJava(String modifier){
		return getJavaName(modifier);
	}
	
	public String getType(){
		return this.class_ass;
	}
	
	public boolean isSame(String name){
		return carma_attribute.equals(name);
	}
	
	public void setPrefix(String prefix){
		this.prefix = prefix;
	}
	
	public String getPrefix(){
		return this.prefix;
	}
	
	public void addValue(int hashCode, Object value){
		CarmaVariableAssignment toAssign = new CarmaVariableNull();
		if(value instanceof EnumAssignmentCarmaInteger){
			CarmaVariableInteger cv = new CarmaVariableInteger();
			Object v = ((CarmaInteger) ((EnumAssignmentCarmaInteger) value).getNaturalValue()).getValue();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof EnumAssignmentMethodReference){	
			CarmaVariableMethodRefence cv = new CarmaVariableMethodRefence();
			Object v = ((EnumAssignmentMethodReference) value).getMethod();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof EnumAssignmentRange){
			CarmaVariableRange cv = new CarmaVariableRange();
			ArrayList<Integer> range = new ArrayList<Integer>();
			range.add(((Range) ((EnumAssignmentRange) value).getRange()).getMin());
			range.add(((Range) ((EnumAssignmentRange) value).getRange()).getMin());
			Object v = range;
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof EnumAssignmentVariableName){
			CarmaVariableReference cv = new CarmaVariableReference();
			Object v = ((VariableName) ((EnumAssignmentVariableName) value).getRef().getName()).getName();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof DoubleAssignmentCarmaDouble){
			CarmaVariableDouble cv = new CarmaVariableDouble();
			Object v = ((DoubleAssignmentCarmaDouble) value).getDoubleValue();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof DoubleAssignmentMethodReference){
			CarmaVariableMethodRefence cv = new CarmaVariableMethodRefence();
			Object v = ((DoubleAssignmentMethodReference) value).getMethod();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof DoubleAssignmentVariableName){
			CarmaVariableReference cv = new CarmaVariableReference();
			Object v = ((VariableName) ((DoubleAssignmentVariableName) value).getReference().getName()).getName();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof IntegerAssignmentCarmaInteger){
			CarmaVariableDouble cv = new CarmaVariableDouble();
			Object v = ((IntegerAssignmentCarmaInteger) value).getIntegerValue();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof IntegerAssignmentMethodReference){
			CarmaVariableMethodRefence cv = new CarmaVariableMethodRefence();
			Object v = ((IntegerAssignmentMethodReference) value).getMethod();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof IntegerAssignmentVariableName){
			CarmaVariableReference cv = new CarmaVariableReference();
			Object v = ((VariableName) ((IntegerAssignmentVariableName) value).getReference().getName()).getName();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} else if (value instanceof RecordDeclaration){
			CarmaVariableReference cv = new CarmaVariableReference();
			Object v = ((RecordName) ((RecordDeclaration) value).getName()).getName();
			setValue(assigns,toAssign,cv,v,hashCode);
			
		} 
		
	}
	
	private void setValue(HashMap<Integer,CarmaVariableAssignment> assigns, 
			CarmaVariableAssignment toAssign, 
			CarmaVariableAssignment cv, 
			Object value,
			int hashCode){
		
		if(cv.setValue(value)){
			assigns.put(hashCode,cv);
		} else {
			assigns.put(hashCode,toAssign);
		}
	}
	
	public boolean isGlobal(){
		return this.isGlobal;
	}
	
	public String getValueAsString(int hashCode){
		return this.assigns.get(hashCode).getValue()+"";
	}
	
}