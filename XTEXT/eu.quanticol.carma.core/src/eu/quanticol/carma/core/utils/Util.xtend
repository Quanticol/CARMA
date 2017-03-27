package eu.quanticol.carma.core.utils

import eu.quanticol.carma.core.carma.Activity
import eu.quanticol.carma.core.carma.AssignmentCommand
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.carma.AttributeReference
import eu.quanticol.carma.core.carma.BlockCommand
import eu.quanticol.carma.core.carma.BooleanType
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.ConstantDefinition
import eu.quanticol.carma.core.carma.CustomType
import eu.quanticol.carma.core.carma.EnumCase
import eu.quanticol.carma.core.carma.EnumDefinition
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.ForCommand
import eu.quanticol.carma.core.carma.ForEach
import eu.quanticol.carma.core.carma.FunctionCommand
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.GlobalContext
import eu.quanticol.carma.core.carma.IfThenElseCommand
import eu.quanticol.carma.core.carma.IntegerType
import eu.quanticol.carma.core.carma.IterationVariable
import eu.quanticol.carma.core.carma.LabelDefinition
import eu.quanticol.carma.core.carma.ListType
import eu.quanticol.carma.core.carma.LocAttribute
import eu.quanticol.carma.core.carma.LocationType
import eu.quanticol.carma.core.carma.LocationVariable
import eu.quanticol.carma.core.carma.LoopingVariable
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.MyContext
import eu.quanticol.carma.core.carma.NamedNode
import eu.quanticol.carma.core.carma.NodePattern
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.ProcessType
import eu.quanticol.carma.core.carma.Processes
import eu.quanticol.carma.core.carma.RealType
import eu.quanticol.carma.core.carma.ReceiverContext
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.Reference
import eu.quanticol.carma.core.carma.ReferenceableElement
import eu.quanticol.carma.core.carma.ReferenceableType
import eu.quanticol.carma.core.carma.ReturnCommand
import eu.quanticol.carma.core.carma.SenderContext
import eu.quanticol.carma.core.carma.SetType
import eu.quanticol.carma.core.carma.SpaceDefinition
import eu.quanticol.carma.core.carma.StoreAttribute
import eu.quanticol.carma.core.carma.SystemDefinition
import eu.quanticol.carma.core.carma.UniverseElement
import eu.quanticol.carma.core.carma.UntypedVariable
import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.UpdateCollectionAdd
import eu.quanticol.carma.core.carma.UpdateCommand
import eu.quanticol.carma.core.carma.ValueType
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.VariableDeclarationCommand
import java.util.LinkedList
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.EObject

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.simulator.space.SpaceModel
import eu.quanticol.carma.core.carma.ConnectionDeclaration
import eu.quanticol.carma.core.carma.ConnectionIfThenElseCommand
import eu.quanticol.carma.core.carma.EdgeProperty
import eu.quanticol.carma.core.carma.ConnectionForLoop
import eu.quanticol.carma.core.carma.ConnectionBlockCommand
import eu.quanticol.carma.core.carma.NodeDeclaration
import eu.quanticol.carma.core.carma.NodeIfThenElseCommand
import eu.quanticol.carma.core.carma.NodeForLoop
import eu.quanticol.carma.core.carma.NodeBlockCommand
import java.util.HashSet
import java.util.HashMap
import eu.quanticol.carma.core.carma.RandomExpression
import eu.quanticol.carma.core.carma.ForLoop
import eu.quanticol.carma.core.carma.UpdateArrayElement
import eu.quanticol.carma.core.carma.UpdateCollectionRemove

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
	public static final String ACT_NAME_PREFIX = "__ACT_NAME__";
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
	
	def spaceName( SpaceDefinition s ) {
		'''get_SPACE_«s.name»'''
	}
	
	def getLabelsAndFeatures( Model m ) {
		m.elements.filter(typeof(SpaceDefinition)).map[ it.labels+it.universe ].flatten
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
	
	def getSpaceModels( Model m ) {
		m.elements.filter(typeof(SpaceDefinition))
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
	
	def CharSequence  toJavaType( ValueType ft ) {
		switch ft {
			IntegerType: '''Integer'''
			RealType: '''Double'''
			BooleanType: '''Boolean'''
			SetType: '''HashSet<«ft.arg.toJavaType»>'''
			ListType: '''LinkedList<«ft.arg.toJavaType»>'''
			LocationType: '''Node'''
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
		'''«CONST_PREFIX»«name»'''
	}

	def  enumCaseName( String name ) {
		'''«ENUM_CASE_PREFIX»«name»'''
	}

	def  stateName( String name , String component ) {
		'''«STATE_PREFIX»_«component»_«name»'''
	}

	def  actionIndexName( String name ) {
		'''«ACT_PREFIX»«name»'''
	}
	
	def actionName( String name ) {
		'''«ACT_NAME_PREFIX»«name»'''	
	}
	
	def attributeName( AttributeReference r ) {
		switch r {
			StoreAttribute: r.reference.name
			LocAttribute: "loc"
		}
	}

	def  attributeName( CharSequence name , ReferenceContext context ) {
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
			IterationVariable: element.name.variableName
			UntypedVariable: element.name.variableName
			AttributeDeclaration: element.name.attributeName(context)
			FunctionDefinition: element.name.functionName
			EnumCase: {
				var parent = element.getContainerOfType(typeof(EnumDefinition))
				if (parent != null) {
					'''«parent.name.enumClass».«element.name.enumCaseName»'''
				} else {
					element.name.enumCaseName
				}							
			}
			ConstantDefinition: element.name.constantName
			ProcessState: component.carmaProcessCreation(element.name)
//			MeasureVariableDeclaration: element.name.variableName
			LabelDefinition: '''CarmaSystem.getCurrentSpaceModel().getLabel( "«element.name»" )'''
			LocationVariable: element.name.variableName
			LoopingVariable: element.name.variableName
			MeasureDefinition: element.name.measureName
			NamedNode: '''CarmaSystem.getCurrentSpaceModel().getVertex( "«element.name»" )'''
		}
	}
	
	def  referencedAttibutes( Expression e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		if (e != null) {
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
		}
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
	
	def getExpression( UpdateCommand c ) {
		switch c {
			UpdateAssignment: c.expression
			UpdateCollectionAdd: c.expression
			UpdateArrayElement: c.expression
		}
	}
	
	def  referencedAttributes( Update e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		var referenced = e.updateAssignment
			.map[ it.expression ]
			.map[ it.referencedAttibutes ]
			.flatten
		referenced = referenced+e.updateAssignment
			.filter(typeof(UpdateArrayElement))
			.map[ it.target ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced = referenced+e.updateAssignment
			.filter(typeof(UpdateCollectionAdd))
			.map[ it.target ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced = referenced+e.updateAssignment
			.filter(typeof(UpdateCollectionRemove))
			.map[ it.target ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced.forEach[ a |
				if (result.forall[ it.name != a.name ]) {
					result.add( a )
				}
			]
		result
	}
	

	def  myAttributes( Update e ) {
		val LinkedList<AttributeDeclaration> result = newLinkedList()
		var referenced = e.updateAssignment.map[ 
			it.expression
		].map[ 
			it.myAttributes
		].flatten
		referenced = referenced + e.updateAssignment
			.filter(typeof(UpdateArrayElement))
			.map[ it.target ]
			.filter(typeof(MyContext))
			.map[ it.attribute ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced = referenced + e.updateAssignment
			.filter(typeof(UpdateCollectionAdd))
			.map[ it.target ]
			.filter(typeof(MyContext))
			.map[ it.attribute ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced = referenced + e.updateAssignment
			.filter(typeof(UpdateCollectionRemove))
			.map[ it.target ]
			.filter(typeof(MyContext))
			.map[ it.attribute ]
			.filter(typeof(StoreAttribute))
			.map[ it.reference ]
		referenced.forEach[ a |
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
		if (e != null) {
			if (e instanceof MyContext) {
				var a = e.attribute
				if (a instanceof StoreAttribute) {
					result.add( a.reference )
				}
			}
			e.getAllContentsOfType(typeof(MyContext)).map[
				it.attribute
			].filter(typeof(StoreAttribute)).map[it.reference].forEach[ a |
				if (result.forall[ it.name != a.name ]) {
					result.add( a )
				}
			]			
		}
		result
	}
	
	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( BlockCommand c , ReferenceContext context ) {
		c.commands.map[ it.attributesInFunctionCommand( context ) ].flatten
	}
	
	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( IfThenElseCommand c , ReferenceContext context ) {
		(c.condition ?. attributesInExpression(context) ) +
		(c.thenBlock.attributesInFunctionCommand( context )	)+
			(c.elseBlock ?. attributesInFunctionCommand( context ) ?: newLinkedList())
	}

	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( ReturnCommand c , ReferenceContext context ) {
		c.expression.attributesInExpression(context)
	}
	
	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( VariableDeclarationCommand c , ReferenceContext context ) {
		c.value ?. attributesInExpression(context) ?: newLinkedList()
	}
	
	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( ForCommand c , ReferenceContext context ) {
		(c.start ?. attributesInExpression(context) ?: newLinkedList())+
			(c.end ?. attributesInExpression(context) ?: newLinkedList())+
				(c.step ?.attributesInExpression(context) ?: newLinkedList())+
					c.body.attributesInFunctionCommand( context )
	}

	def dispatch Iterable<AttributeDeclaration> attributesInFunctionCommand( AssignmentCommand c , ReferenceContext context ) {
		c.value.attributesInExpression(context)	
	}
	
	def attributesInExpression( Expression e , ReferenceContext context ) {
		switch context {
			case GLOBAL: e.globalAttributes
			case MY: e.myAttributes
			case SENDER: e.senderAttributes
			case RECEIVER: e.receiverAttributes
			case NONE: e.referencedAttibutes
		}
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
		if (e instanceof GlobalContext) {
			result.add( e.reference )
		}
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
		if (e instanceof SenderContext) {
			var a = e.attribute
			if (a instanceof StoreAttribute) {
				result.add( a.reference )
			}
		}
		e.getAllContentsOfType(typeof(SenderContext)).map[
			it.attribute
		].filter(typeof(StoreAttribute)).map[it.reference].forEach[ a |
			if (result.forall[ it.name != a.name ]) {
				result.add( a )
			}
		]
		result
	}
	
	def useSenderLoc( Expression e) {
		if (e instanceof SenderContext) {
			if (e.attribute instanceof LocAttribute) {
				true
			}
		} else {
			e.getAllContentsOfType(typeof(SenderContext)).map[
				it.attribute
			].filter(typeof(LocAttribute)).size > 0
		}
	}
	
	def useReceiverLoc( Expression e) {
		if (e instanceof ReceiverContext) {
			if (e.attribute instanceof LocAttribute) {
				true
			}
		} else {
			e.getAllContentsOfType(typeof(ReceiverContext)).map[
				it.attribute
			].filter(typeof(LocAttribute)).size > 0
		}
	}
	
	def useMyLoc( Expression e) {
		if (e instanceof MyContext) {
			if (e.attribute instanceof LocAttribute) {
				true
			}
		} else {
			e.getAllContentsOfType(typeof(MyContext)).map[
				it.attribute
			].filter(typeof(LocAttribute)).size > 0
		}
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
		if (e instanceof ReceiverContext) {
			var a = e.attribute
			if (a instanceof StoreAttribute) {
				result.add( a.reference )
			}
		}
		e.getAllContentsOfType(typeof(ReceiverContext)).map[
			it.attribute
		].filter(typeof(StoreAttribute)).map[it.reference].forEach[ a |
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
	
	
	def dispatch boolean doReturn( ReturnCommand c ) {
		true
	}
	
	def dispatch boolean doReturn( IfThenElseCommand c ) {
		(c.thenBlock !=null)&&(c.thenBlock.doReturn)&&(c.elseBlock != null)&&(c.elseBlock.doReturn)
	}
	
	def dispatch boolean doReturn( ForCommand c ) {
		(c.body != null)&&(c.body.doReturn)
	}

	def dispatch boolean doReturn( BlockCommand c ) {
		c.commands.exists[ it.doReturn ]
	}
	
	def dispatch boolean doReturn( FunctionCommand c ) {
		false
	}
	
	def indexOfLocationVariable( LocationVariable v ) {
		var pattern = v.getContainerOfType( typeof(NodePattern) )
		if (pattern != null) {
			pattern.elements.indexOf( v ) 
		} else {
			-1
		}
	}
	
	def generateList( String v , int size ) {
		var result = newLinkedList()
		var counter = 0;
		while (counter<size) {
			result.add(v+counter);
			counter = counter+1;
		}
		result;	
	}

	def Iterable<? extends ReferenceableElement> variablesDeclaredBefore( FunctionCommand c ) {
		c ?. eContainer ?. variablesDeclaredBefore( c ) ?: newLinkedList()
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( IfThenElseCommand c1 , FunctionCommand c2 ) {
		c1.variablesDeclaredBefore
	}
	
	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( ForCommand c1 , FunctionCommand c2 ) {
		c1.variablesDeclaredBefore + newLinkedList( c1.variable )
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( ForEach c1 , FunctionCommand c2 ) {
		c1.variablesDeclaredBefore + newLinkedList( c1.iteration )
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( ReturnCommand c1 , FunctionCommand c2 ) {
		newLinkedList(  )
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( AssignmentCommand c1 , FunctionCommand c2 ) {
		newLinkedList(  )
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( VariableDeclarationCommand c1 , FunctionCommand c2 ) {
		newLinkedList( )
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( BlockCommand c1 , FunctionCommand c2 ) {
		var idx = c1.commands.indexOf( c2 )		
		if (idx >= 0) {
			c1.variablesDeclaredBefore + c1.commands.subList(0,idx).filter(typeof(VariableDeclarationCommand)).map[it.variable]
		} else {
			c1.variablesDeclaredBefore
		}
	}

	def dispatch Iterable<? extends ReferenceableElement> variablesDeclaredBefore( EObject o , FunctionCommand c ) {
		newLinkedList( )
	}

	def isReferenceToNodeName( EObject o ) {
		if (o instanceof Reference) {
			if (o.reference instanceof NamedNode) {
				true
		 	} else {
		 		false
		 	}
		} else {
			false
		}
	}
	
	def getIndexOf( UniverseElement e ) {
		var sm = e.getContainerOfType(typeof(SpaceDefinition))
		return sm.universe.indexOf(e)
	}

	def edgeAttributes( SpaceDefinition model ) {
		val eProperties = model.edges.map[ it.declaredEdgeAttrbutes ].flatten
		eProperties.filter[ x | x == eProperties.findFirst[ it.name == x.name ]]
	}
	
	def dispatch declaredEdgeAttrbutes( ConnectionDeclaration c ) {
		c.edgeProperties
	}
	
	def dispatch Iterable<EdgeProperty> declaredEdgeAttrbutes( ConnectionIfThenElseCommand c ) {
		c.thenBlock.declaredEdgeAttrbutes+c.elseBlock.declaredEdgeAttrbutes
	}
	
	def dispatch Iterable<EdgeProperty> declaredEdgeAttrbutes( ConnectionForLoop c ) {
		c.body.declaredEdgeAttrbutes
	}

	def dispatch Iterable<EdgeProperty> declaredEdgeAttrbutes( ConnectionBlockCommand c ) {
		c.edges.map[ it.declaredEdgeAttrbutes ].flatten
	}
	
	def nodeNames( SpaceDefinition model ) {
		val nNames = model.nodes.map[it.declaredNamedNodes].flatten
		nNames.filter[ x | x == nNames.findFirst[ it.name == x.name ]]
	}
	
	
	def dispatch Iterable<NamedNode>  declaredNamedNodes( NodeDeclaration n ) {
		if (n instanceof NamedNode) {
			newLinkedList( n )
		} else {
			newLinkedList()
		}
	}

	def dispatch Iterable<NamedNode>  declaredNamedNodes( NodeIfThenElseCommand n ) {
		if (n.elseBlock != null) {
			n.thenBlock.declaredNamedNodes+n.elseBlock.declaredNamedNodes
		} else {
			n.thenBlock.declaredNamedNodes
		}
	}

	def dispatch Iterable<NamedNode>  declaredNamedNodes( NodeForLoop n ) {
		n.body.declaredNamedNodes
	}

	def dispatch Iterable<NamedNode>  declaredNamedNodes( NodeBlockCommand n ) {
		n.nodes.map[ it.declaredNamedNodes ].flatten
	}

	def areaNames( SpaceDefinition model ) {
		model.labels
	}
	
	def invokedFunctions( FunctionDefinition f ) {
		f.body.getAllContentsOfType( typeof(Reference) ).filter[ it.isIsCall ].map[ it.reference ].filter(typeof(FunctionDefinition))
	}
	
	def isRecursive( FunctionDefinition f ) {
		val invoked = newHashSet( )
		invoked.addAll( f.invokedFunctions )
		var toExpand = invoked
		while (!toExpand.empty) {
			var nextToExpand = newHashSet()
			for (g: toExpand) {
				nextToExpand.addAll(g.invokedFunctions.filter[!invoked.contains(it)])		
			}
			toExpand = nextToExpand
			invoked.addAll(toExpand)
		}
		invoked.contains(f)
	}
	
	def recursiveFunctions( Model m ) {
		m.elements.filter(typeof(FunctionDefinition)).filter[it.isRecursive]
	}
	
	def dispatch boolean isARandomCommand( IfThenElseCommand c ) {
		if ((c.condition != null)&&(c.condition.usesRandomExpressions)) {
			true
		} else {
			((c.thenBlock != null)&&(c.thenBlock.isARandomCommand))||
			((c.elseBlock != null)&&(c.elseBlock.isARandomCommand))
		}
	}

	def dispatch boolean isARandomCommand( ReturnCommand c ) {
		(c.expression != null)&&(c.expression.usesRandomExpressions)
	}

	def dispatch boolean isARandomCommand( VariableDeclarationCommand c ) {
		(c.value != null)&&(c.value.usesRandomExpressions)
	}

	def dispatch boolean isARandomCommand( ForCommand c ) {
		((c.start!=null)&&(c.start.usesRandomExpressions))||((c.step!=null)&&(c.step.usesRandomExpressions))||((c.end != null)||(c.end.usesRandomExpressions))
	}

	def dispatch Boolean isARandomCommand( ForEach c ) {
		(c.iteration != null)&&(c.iteration.value!=null)&&(c.iteration.value.usesRandomExpressions)
	}

	def dispatch Boolean isARandomCommand( BlockCommand c ) {
		c.commands.exists[it.isARandomCommand]
	}
	
	def dispatch Boolean isARandomCommand( AssignmentCommand c ) {
		(c.value != null)&&(c.value.usesRandomExpressions)
	}
	
	def referencedFunctions( EObject o ) {
		var list = o.getAllContentsOfType(typeof(Reference))
		if (o instanceof Reference) {
			list.add( o )
		}
		list.filter[it.isIsCall].map[it.reference].filter(typeof(FunctionDefinition))
	}
		
	def void fillRecursiveTable( FunctionDefinition f , HashSet<FunctionDefinition> pending , HashMap<FunctionDefinition,Boolean> recursiveTable ) {
		if (recursiveTable.get(f)==null) {
			pending.add( f )
			var referencedFunctions = f.referencedFunctions
			referencedFunctions.filter[!pending.contains(it)].forEach[
				it.fillRecursiveTable(pending,recursiveTable)
			]
			if (referencedFunctions.exists[pending.contains(it)||((recursiveTable.get(it)!=null)&&(recursiveTable.get(it)))]) {
				recursiveTable.put(f,true)
			} else {
				recursiveTable.put(f,false)
			}
			pending.remove( f )
		}		
	}
	
	def buildRecursiveTable( Model m ) {
		val table = newHashMap()
		val set = newHashSet() 
		m.elements.filter(typeof(FunctionDefinition)).forEach[it.fillRecursiveTable(set,table)]
		table
	}
	
	def buildRandomFuncitonTable( Model m ) {
		val table = newHashMap()
		m.elements.filter(typeof(FunctionDefinition)).forEach[
			if (it.body == null) {
				table.put(it,false)
			} else {
				table.put(it,it.body.isARandomCommand)				
			}
			
		]
		table
	}
	
	def usesRandomExpressions( Expression e ) {
		(e instanceof RandomExpression)||(!e.getAllContentsOfType(typeof(RandomExpression)).empty)
	}
	
	def isRandom( Expression e , HashMap<FunctionDefinition,Boolean> randomTable ) {
		(e.usesRandomExpressions)||(e.referencedFunctions.exists[(randomTable.get(it)!=null)&&(randomTable.get(it))])
	}
	
	def isRecursive( Expression e , HashMap<FunctionDefinition,Boolean> recursiveTable ) {
		e.referencedFunctions.exists[(recursiveTable.get(it)!=null)&&(recursiveTable.get(it))]	
	}
	
	
 }