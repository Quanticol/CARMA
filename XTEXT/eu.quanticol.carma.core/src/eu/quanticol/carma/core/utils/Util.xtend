package eu.quanticol.carma.core.utils

import com.google.inject.Inject
import java.util.HashSet
import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.ArrayList
import eu.quanticol.carma.core.carma.ProcessComposition
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
import eu.quanticol.carma.core.carma.ReferenceableType
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.Activity
import eu.quanticol.carma.core.carma.Element
import eu.quanticol.carma.core.carma.SystemDefinition
import eu.quanticol.carma.core.carma.StoreBlock
import org.eclipse.xtext.scoping.Scopes
import org.eclipse.xtext.scoping.IScope
import org.eclipse.emf.common.util.EList
import java.util.Set
import java.util.LinkedList
import eu.quanticol.carma.core.typing.CarmaType
import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.Reference
import eu.quanticol.carma.core.carma.MyContext
import eu.quanticol.carma.core.carma.GlobalContext
import eu.quanticol.carma.core.carma.SenderContext
import eu.quanticol.carma.core.carma.ReceiverContext
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration

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
	public static final String MEASURE_PREFIX = "__MEASURE__";
	public static final String SYSTEM_PREFIX = "__SYSTEM__";
	
	def systemName( String name ) {
		'''«SYSTEM_PREFIX»«name»'''
	}
	
	def carmaProcessCreation( String component , String state ) {
		'''new CarmaSequentialProcess( _COMP_«component» , «state.stateName(component)» )'''
	}
	
	def  getActivities( Model m ) {
		var activities = m.getAllContentsOfType(typeof(Activity))
		val LinkedList<Activity> toReturn = newLinkedList()
		activities.forEach[ a |
			if (toReturn.forall[ !it.name.equals(a.name) ]) {
				toReturn.add(a)
			}
		]
		toReturn
	}	
	
	def  getActivities( Model m , boolean broadcast ) {
		var activities = m.getAllContentsOfType(typeof(Activity)).filter[ it.isBroadacst==broadcast ]
		val LinkedList<Activity> toReturn = newLinkedList()
		activities.forEach[ a |
			if (toReturn.forall[ !it.name.equals(a.name) ]) {
				toReturn.add(a)
			}
		]
		toReturn
	}
	
	def  getMessages( Model m , Activity a ) {
		m.getAllContentsOfType(typeof(OutputAction)).filter[ 
			(it.activity.name == a.name)&&(it.activity.isIsBroadacst==a.isIsBroadacst)
		].map[it.outputArguments]
	}
	
	def  getAllAttributes( Model m ) {
		m.components.map[it.store.attributes].flatten
		
	}
	
	def  getAllGlobalAttributes( Model m ) {
		m.systems.filter[it.environment != null].
			filter[ it.environment.store != null].
				map[ it.environment.store.attributes ].flatten
	}
	
	def  getGlobalAttributes( Model m ) {
		var sys = m.systems
		val toReturn = newLinkedList()
		sys.filter[it.environment != null].
				filter[ it.environment.store != null].
					forEach[
						toReturn.mergeAttributes(it.environment.store.attributes)
					]
		toReturn
	}
	
	def getEnums( Model m ) {
		m.elements.filter(typeof(EnumDefinition))
	}
	
	def  getAttributes( Model m ) {
		var comps = m.components
		val toReturn = newLinkedList()
		comps.forEach[
			toReturn.mergeAttributes(it.store.attributes)
		]
		toReturn
	}
	
	def  mergeAttributes( LinkedList<AttributeDeclaration> set , EList<AttributeDeclaration> attrs ) {
		attrs.forEach[
			a | if (set.forall[ !it.name.equals(a.name) ]) { set.add(a) }
		]
	}
	
	def  getFunctions( Model m ) {
		m.elements.filter(typeof(FunctionDefinition)) 		
	}
	
	def  getRecords( Model m ) {
		m.elements.filter(typeof(RecordDefinition)) 		
	}
	
	def  getFields( Model m ) {
		m.elements.filter(typeof(RecordDefinition)).map[it.fields].flatten 		
	}
	
	def  getGlobalProcesses( Model m ) {
		m.elements.filter(typeof(Processes)).map[it.processes].flatten	
	}

	def  getAllProcesses( Model m ) {
		m.elements.filter(typeof(Processes)).map[it.processes].flatten	
			+m.elements.filter(typeof(ComponentDefinition)).map[it.processes.processes].flatten
	}
	
	def  getConstants( Model m ) {
		m.elements.filter(typeof(ConstantDefinition))
	}
	
	def  getComponents( Model m ) {
		m.elements.filter(typeof(ComponentDefinition))
	}
	
	def  getMeasures( Model m ) {
		m.elements.filter(typeof(MeasureDefinition))
	}

	def  getSystems( Model m ) {
		m.elements.filter(typeof(SystemDefinition))
	}
	
	
	def  enumClass( String name ) {
		'''«ENUM_PREFIX»«name»'''
	}
	
	def  recordClass( String name ) {
		'''«RECORD_PREFIX»«name»'''
	}
	
	def measureName( String name ) {
		'''«MEASURE_PREFIX»«name»'''
	}
	
	def  customToJavaType( ReferenceableType t ) {
		switch t {
			EnumDefinition: t.name.enumClass
			RecordDefinition: t.name.recordClass
		}
	}
	
	
	def  toJavaDeclaration( Variable v ) {
		'''«v.type.toJavaType» «v.name.variableName»'''
	}
	
	def  toJavaType( ValueType ft ) {
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
			ProcessType: '''CarmaProcess'''
		}
	}
	
	def  fieldName( String name ) {
		'''«FIELD_PREFIX»«name»'''
	}
	
	def  variableName( String name ) {
		'''«VAR_PREFIX»«name»'''
	}

	def  functionName( String name ) {
		'''«FUN_PREFIX»«name»'''
	}

	def  constantName( String name ) {
		'''«FUN_PREFIX»«name»'''
	}

	def  enumCaseName( String name ) {
		'''«CONST_PREFIX»«name»'''
	}

	def  stateName( String name , String component ) {
		'''«STATE_PREFIX»_«component»_«name»'''
	}

	def  actionName( String name ) {
		'''«ACT_PREFIX»«name»'''
	}

	def  attributeName( String name , ReferenceContext context ) {
		switch context {
			case NONE: '''«ATTR_PREFIX»«name»'''
			case MY: '''«MY_PREFIX»«name»'''
			case GLOBAL: '''«GLOBAL_PREFIX»«name»'''			
			case SENDER: '''«SENDER_PREFIX»«name»'''			
			case RECEIVER: '''«RECEIVER_PREFIX»«name»'''			
		}
	}
	
	def  getReference( ReferenceableElement element , ReferenceContext context , String component ) {
		switch element {
			Variable: element.name.variableName
			UntypedVariable: element.name.variableName
			AttributeDeclaration: element.name.attributeName(context)
			FunctionDefinition: element.name.functionName
			EnumCase: element.name.enumCaseName			
			ConstantDefinition: element.name.constantName
			ProcessState: component.carmaProcessCreation(element.name)
			MeasureVariableDeclaration: element.name.variableName
		}
	}
	
	def  referencedAttibutes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		if (e instanceof Reference) {
			if (e.reference instanceof AttributeDeclaration) {
				result.add( e.reference as AttributeDeclaration )
			}
		}
		e.getAllContentsOfType(typeof(Reference)).map[ 
			it.reference
		].filter(typeof(AttributeDeclaration)).forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	} 

	def referencedAttibutes( Iterable<Expression> e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.map[ it.referencedAttibutes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def  referencedAttributes( Update e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.updateAssignment.map[ it.expression ].map[ it.referencedAttibutes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}

	def  myAttributes( Update e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.updateAssignment.map[ 
			it.expression
		].map[ 
			it.myAttributes
		].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def myAttributes( Iterable<Expression> e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.map[ it.myAttributes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	

	def  myAttributes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.getAllContentsOfType(typeof(MyContext)).map[
			it.reference
		].forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	
	def globalAttributes( Iterable<Expression> e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.map[ it.globalAttributes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def  globalAttributes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.getAllContentsOfType(typeof(GlobalContext)).map[
			it.reference
		].forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def  senderAttributes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.getAllContentsOfType(typeof(SenderContext)).map[
			it.reference
		].forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def senderAttributes( Iterable<Expression> e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.map[ it.senderAttributes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	
	
	def  receiverAttributes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.getAllContentsOfType(typeof(ReceiverContext)).map[
			it.reference
		].forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def receiverAttributes( Iterable<Expression> e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		e.map[ it.receiverAttributes ].flatten.forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
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