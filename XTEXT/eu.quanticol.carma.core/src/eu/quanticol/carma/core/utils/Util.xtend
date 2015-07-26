package eu.quanticol.carma.core.utils

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Process
import java.util.HashSet
import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.ArrayList
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.carma.ProcessParameter
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import org.eclipse.emf.ecore.EObject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.Processes
import eu.quanticol.carma.core.carma.ConstantDefinition
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.carma.FieldType
import eu.quanticol.carma.core.carma.IntegerType
import eu.quanticol.carma.core.carma.RealType
import eu.quanticol.carma.core.carma.BooleanType
import eu.quanticol.carma.core.carma.CustomType
import eu.quanticol.carma.core.carma.EnumDefinition
import eu.quanticol.carma.core.carma.ReferenceableElement
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.UntypedVariable
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.carma.EnumCase
import eu.quanticol.carma.core.carma.ProcessType
import eu.quanticol.carma.core.carma.ValueType
import eu.quanticol.carma.core.carma.IntegerParameter
import eu.quanticol.carma.core.carma.RealParameter
import eu.quanticol.carma.core.carma.BooleanParameter
import eu.quanticol.carma.core.carma.CustomParameter
import eu.quanticol.carma.core.carma.ReferenceableType
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.Activity

class Util {

	public static final String RECORD_PREFIX = "__RECORD__";
	public static final String FIELD_PREFIX = "__FIELD__";
	public static final String ENUM_PREFIX = "__ENUM__";
	public static final String ENUM_CASE_PREFIX = "__CASE__";
	public static final String CONST_PREFIX = "__CONST__";
	public static final String VAR_PREFIX = "__VARIABLE__";
	public static final String FUN_PREFIX = "__FUN__";
	public static final String ATTR_PREFIX = "__ATTR__";
	public static final String MY_PREFIX = "__MY__";
	public static final String GLOBAL_PREFIX = "__GLOBAL__";
	public static final String SENDER_PREFIX = "__SENDER__";
	public static final String RECEIVER_PREFIX = "__RECEIVER__";
	public static final String STATE_PREFIX = "__STATE__";
	public static final String ACT_PREFIX = "__ACT__";
	
	
	def static getFunctions( Model m ) {
		m.elements.filter(typeof(FunctionDefinition)) 		
	}
	
	def static getRecords( Model m ) {
		m.elements.filter(typeof(RecordDefinition)) 		
	}
	
	def static getFields( Model m ) {
		m.elements.filter(typeof(RecordDefinition)).map[it.fields].flatten 		
	}
	
	def static getProcesses( Model m ) {
		m.elements.filter(typeof(Processes)).map[it.processes].flatten	
	}
	
	def static getConstants( Model m ) {
		m.elements.filter(typeof(ConstantDefinition))
	}
	
	def static getComponents( Model m ) {
		m.elements.filter(typeof(ComponentDefinition))
	}
	
	def static getMeasures( Model m ) {
		m.elements.filter(typeof(MeasureDefinition))
	}
	
	def static enumClass( String name ) {
		'''«ENUM_PREFIX»«name»'''
	}
	
	def static recordClass( String name ) {
		'''«RECORD_PREFIX»«name»'''
	}
	
	
	def static customToJavaType( ReferenceableType t ) {
		switch t {
			EnumDefinition: t.name.enumClass
			RecordDefinition: t.name.recordClass
		}
	}
	
	
	def static toJavaDeclaration( Variable v ) {
		switch v {
			IntegerParameter: '''Integer «v.name.variableName»'''
			RealParameter: '''Double «v.name.variableName»'''
			BooleanParameter: '''Boolean «v.name.variableName»'''
			ProcessParameter: '''Object «v.name»''' //FIXME!!!
			CustomParameter: {
				var ref = v.reference
				switch ref {
					EnumDefinition: '''«ref.name.enumClass» «v.name.variableName»'''							
					RecordDefinition: '''«ref.name.recordClass» «v.name.variableName»'''							
				}
			
			}
		}
	}
	
	def static toJavaType( ValueType ft ) {
		switch ft {
			IntegerType: '''Integer'''
			RealType: '''Double'''
			BooleanType: '''Boolean'''
			CustomType: {
				var ref = ft.reference
				switch ref {
					RecordDefinition: ref.name.recordClass
					EnumDefinition: ref.name.enumClass
				}
			}
			ProcessType: '''Object'''
		}
	}
	
	def static fieldName( String name ) {
		'''«FIELD_PREFIX»«name»'''
	}
	
	def static variableName( String name ) {
		'''«VAR_PREFIX»«name»'''
	}

	def static functionName( String name ) {
		'''«FUN_PREFIX»«name»'''
	}

	def static constantName( String name ) {
		'''«FUN_PREFIX»«name»'''
	}

	def static enumCaseName( String name ) {
		'''«CONST_PREFIX»«name»'''
	}

	def static stateName( String name , String component ) {
		'''«STATE_PREFIX»_«component»_«name»'''
	}

	def static actionName( String name ) {
		'''«ACT_PREFIX»«name»'''
	}

	def static attributeName( String name , ReferenceContext context ) {
		switch context {
			case NONE: '''«ATTR_PREFIX»«name»'''
			case MY: '''«MY_PREFIX»«name»'''
			case GLOBAL: '''«GLOBAL_PREFIX»«name»'''			
			case SENDER: '''«SENDER_PREFIX»«name»'''			
			case RECEIVER: '''«RECEIVER_PREFIX»«name»'''			
		}
	}
	
	def static getReference( ReferenceableElement element , ReferenceContext context ) {
		switch element {
			Variable: element.name.variableName
			UntypedVariable: element.name.variableName
			AttributeDeclaration: element.name.attributeName(context)
			FunctionDefinition: element.name.functionName
			EnumCase: element.name.enumCaseName			
			ConstantDefinition: element.name.constantName	
		}
	}
	
//	def boolean sameName(Name name1, Name name2){
//		name1.name.equals(name2.name)
//	}
//	
//	def ArrayList<Process> getInitialState(CBND cbnd){
//		
//		var definition = cbnd.name.getContainerOfType(ComponentBlockDefinition)
//		var ProcessParameter processParameter = null 
//		if(definition.componentSignature.componentParameters.eAllOfType(ProcessParameter).size > 0)
//			processParameter = definition.componentSignature.componentParameters.eAllOfType(ProcessParameter).get(0)
//			
//		var String parameterLabel = ""
//		if(processParameter != null)
//			parameterLabel = processParameter.name.name
//			
//		var ProcessComposition initialisation = definition.componentBlock.initBlock.init
//		var ArrayList<String> array = new ArrayList<String>()
//		initialisation.stringArray(array)
//		
//		var ArrayList<Process> toReturn = new ArrayList<Process>()
//		
//		initialisation.processArray(toReturn)
//		
//		if(array.contains(parameterLabel)){
//			var i = array.indexOf(parameterLabel)
//			toReturn.remove(i)
//			var ProcessComposition argumentProcessComposition = null 
//			if(cbnd.arguments.eAllOfType(ProcessComposition).size > 0){
//				argumentProcessComposition = cbnd.arguments.eAllOfType(ProcessComposition).get(0)
//				argumentProcessComposition.processArray(toReturn)
//			}
//		}
//		return toReturn
//	}
//	
//	def void stringArray(ProcessComposition processComposition, ArrayList<String> array){
//		switch(processComposition){
//			ParallelComposition	: {processComposition.left.stringArray(array) processComposition.right.stringArray(array)}
//			ProcessReference	: {array.add(processComposition.expression.name)}
//		}
//	}
//	
//	def void processArray(ProcessComposition processComposition, ArrayList<Process> array){
//		switch(processComposition){
//			ParallelComposition	: {processComposition.left.processArray(array) processComposition.right.processArray(array)}
//			ProcessReference	: {array.add(processComposition.expression.getContainerOfType(Process))}
//		}
//	}
//	
//	/**
//	 * Return a List of all Processes associated with the given one. 
//	 * All Processes that transition from, or to, the Process argument. 
//	 * <p>
//	 * @author 	CDW <br>
//	 * @param	Process <br>
//	 * @return	ArrayList<Process>
//	 */
//	def ArrayList<Process> maximumFixedPoint(Process p1, boolean includeSelf){
//		
//		
//		var HashSet<Process> buffer1 = new HashSet<Process>()
//		var HashSet<Process> buffer2 = new HashSet<Process>()
//		
//		if(includeSelf)
//			buffer1.add(p1)
//		else
//			buffer1.addAll(getReferences(p1))
//		
//		while(buffer1.size > buffer2.size){
//			
//			buffer2.addAll(buffer1)
//			
//			for(Process p2 : buffer2){
//				if(includeSelf)
//					buffer1.addAll(getAllReferences(p2))
//				else
//					buffer1.addAll(getReferences(p2))
//			}
//		}
//		
//		var ArrayList<Process> output = new ArrayList<Process>(buffer1)
//		
//		return output
//		
//	}
//	
//	/**
//	 * Return all Processes referenced by the argument Process. Both parents and children.
//	 * 
//	 * @author 	CDW
//	 * @param	Process
//	 * @return	HashSet<Process>
//	 */
//	def HashSet<Process> getAllReferences(Process p1){
//		
//		var HashSet<Process> output = new HashSet<Process>()
//		
//		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
//				if(p1.name.equals(pr.expression.name))
//					output.add(p2)
//		
//		for(ProcessExpressionReference pr : p1.eAllOfType(ProcessExpressionReference))
//			for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//				if(p2.name.equals(pr.expression.name))
//					output.add(p2)
//					
//		return output 
//		
//	}
//	
//	/**
//	 * Return all Processes referenced by the argument Process. Only children.
//	 * 
//	 * @author 	CDW
//	 * @param	Process
//	 * @return	HashSet<Process>
//	 */
//	def HashSet<Process> getReferences(Process p1){
//		
//		var HashSet<Process> output = new HashSet<Process>()
//		
//		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
//				if(p1.name.equals(pr.expression.name))
//					output.add(p2)
//							
//		return output 
//		
//	}
}