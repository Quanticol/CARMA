package eu.quanticol.carma.core.generator.ms.model

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NamedLocationExpression
import eu.quanticol.carma.core.carma.NamedNode
import eu.quanticol.carma.core.carma.NodeBlockCommand
import eu.quanticol.carma.core.carma.NodeForCommand
import eu.quanticol.carma.core.carma.NodeForEach
import eu.quanticol.carma.core.carma.NodeIfThenElseCommand
import eu.quanticol.carma.core.carma.SpaceDefinition
import eu.quanticol.carma.core.carma.UnNamedLocationExpression
import eu.quanticol.carma.core.carma.UnNamedNode
import eu.quanticol.carma.core.carma.UniverseElement
import eu.quanticol.carma.core.generator.ms.activities.ActivityHandler
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.enums.EnumHandler
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.generator.ms.function.FunctionHandler
import eu.quanticol.carma.core.generator.ms.measure.MeasureHandler
import eu.quanticol.carma.core.generator.ms.record.RecordHandler
import eu.quanticol.carma.core.generator.ms.system.SystemHandler
import eu.quanticol.carma.core.typing.TypeSystem
import eu.quanticol.carma.core.utils.Util
import org.eclipse.emf.common.util.EList
import eu.quanticol.carma.core.carma.ConnectionDeclaration
import eu.quanticol.carma.core.carma.ConnectionIfThenElseCommand
import eu.quanticol.carma.core.carma.ConnectionForLoop
import eu.quanticol.carma.core.carma.ConnectionBlockCommand
import eu.quanticol.carma.core.carma.UnDirectedEdge
import eu.quanticol.carma.core.carma.ConnectionForCommand
import eu.quanticol.carma.core.carma.ConnectionForEach
import eu.quanticol.carma.core.carma.AreaElementDeclaration
import eu.quanticol.carma.core.carma.AreaIfThenElseCommand
import eu.quanticol.carma.core.carma.AreaBlockCommand
import eu.quanticol.carma.core.carma.AreaForCommand
import eu.quanticol.carma.core.carma.AreaForEach

class ModelHandler {
	
	@Inject extension EnumHandler
	@Inject extension RecordHandler
	@Inject extension Util
	@Inject extension ActivityHandler
	@Inject extension FunctionHandler
	@Inject extension CollectiveHandler
	@Inject extension SystemHandler
	@Inject extension MeasureHandler
	@Inject extension ExpressionHandler
	@Inject extension TypeSystem
	
	def modelToJava( Model model , String className , String packageName ) {
		'''
		package «packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.*;		
		import eu.quanticol.carma.simulator.*;
		import eu.quanticol.carma.simulator.space.Location;
		import eu.quanticol.carma.simulator.space.Node;
		import eu.quanticol.carma.simulator.space.SpaceModel;
		import eu.quanticol.carma.simulator.space.Tuple;
		import eu.quanticol.carma.simulator.space.Edge;
		import java.util.LinkedList;
		import java.util.HashSet;
		import java.util.HashMap;
		import java.util.Map;
		import java.util.Set;
		import java.util.List;
		import java.util.TreeSet;
		import java.util.Collection;
		import java.util.function.Function;
		import java.util.function.Predicate;
		import org.cmg.ml.sam.sim.sampling.*;
		

		public class «className» extends CarmaModel {
			
			public «className»() {
				«FOR c:model.components»
				«c.methofForComponentBehaviourCreation»;
				«ENDFOR»
			}
			

			«FOR e:model.enums» 
			«e.enumToJava»
			«ENDFOR»
			
			«FOR r:model.records»
			«r.recordToJava»
			«ENDFOR»
		
			«FOR c:model.constants»
			public final «c.value.typeOf.toJavaType(true)» «c.name.constantName» = «c.value.expressionToJava»;
			«ENDFOR»
		
			«FOR f:model.functions»
			«f.function»
			«ENDFOR»
			
			«FOR s:model.spaceModels»
			«s.generateSpaceModelCode»
			«ENDFOR»
			
			«FOR c:model.components»
			«c.componentBehaviour(model.globalProcesses)»
			«ENDFOR»
			
			«model.activities.activitiesToJava»
			
			«model.systems.systemsCollector»
			
			«FOR s:model.systems»
			«s.systemFactory»
			«ENDFOR»
			
			«model.measures.collectMeasures»
			
		}
		'''
	}
	
	
	def generateSpaceModelCode( SpaceDefinition s ) {
		'''
		public SpaceModel «s.spaceName»( «FOR p:s.parameters SEPARATOR ','»  «p.type.toJavaType» «p.name.variableName» «ENDFOR» ) {
			SpaceModel sm = new SpaceModel();
			
			«FOR n:s.nodes»
			«n.vertexInstantiation»
			«ENDFOR»
			
			«FOR e:s.edges»
			«e.generateEdgeCode»
			«ENDFOR»
			
			«FOR l:s.labels»
			{
				Node tmpNode = null;
				HashSet<Node> area = new HashSet<Node>();
				«FOR c:l.nodes»
				«c.generateAreaCode»
				«ENDFOR»
				sm.setArea( "«l.name»" , area );
			}
			«ENDFOR»
«««			for( Node l: sm.getAll() ) {
«««				«FOR e:s.edges»
«««				«s.universe.generateEdgeExpression(e)»
«««				«ENDFOR»
«««			}
		
			return sm;
		}		
		
«««		«FOR f:s.features»
«««		public static «f.type.toJavaType» «f.featureName» (
«««			«FOR p:f.parameters.indexed SEPARATOR ","»
«««			«s.locations.typeOfLocationElement(p.key)» «p.value.name.variableName»
«««			«ENDFOR»
«««		) {
«««			«f.body.functionBodyToJava»
«««		}
«««		«ENDFOR»
		
		'''
	}

	
//	def generateLabelCode(LocationBody locs , LabelDefinition definition) {
//		'''
//		{
//			HashSet<Location> newLabel = new HashSet<>();
//			for (Location l: sm.getAll()) {
//			«FOR p:definition.patterns»
//			«locs.generatePatternCase( p )»
//			«ENDFOR»
//			}
//			sm.setLabel( "«definition.name»" , newLabel );
//		}
//		'''	}
//	
//	def generatePatternCase(LocationBody locs, LabelDefinitionCase c) {
//		var vars = c.pattern.elements.filter(typeof(LocationVariable))
//		var values = c.pattern.elements.filter(typeof(LocationExpressionValue));
//		'''
//		{
//			«FOR v:vars»
//			«locs.typeOfLocationElement(c.pattern.elements.indexOf(v))» «v.name.variableName»  = («locs.typeOfLocationElement(c.pattern.elements.indexOf(v))») l.get( «c.pattern.elements.indexOf(v)» );
//			«ENDFOR»
//			«IF !values.empty»
//			if (
//			«FOR v:values SEPARATOR "&&"»
//				((«v.value.expressionToJava»)==((«locs.typeOfLocationElement(c.pattern.elements.indexOf(v))») l.get( «c.pattern.elements.indexOf(v)» )))
//			«ENDFOR»
//			) {
//			«ENDIF»
//			«IF c.guard !=null»
//				if («c.guard.expressionToJava») {
//			«ENDIF»
//					newLabel.add( l );
//			«IF c.guard !=null»
//				}
//			«ENDIF»
//			«IF !values.empty»
//			}
//			«ENDIF»
//		}
//		'''
//	}
	
	
	def dispatch CharSequence vertexInstantiation( NamedNode n ) {
		'''
		sm.addVertex( "«n.name»" , new Tuple( «FOR v:n.values SEPARATOR ','»«v.expressionToJava»«ENDFOR» ) );
		'''
	}
	
	def dispatch CharSequence vertexInstantiation( UnNamedNode n ) {
		'''
		sm.addVertex( null , new Tuple( «FOR v:n.values SEPARATOR ','»«v.expressionToJava»«ENDFOR» ) );
		'''
	}
	
	def dispatch CharSequence vertexInstantiation( NodeIfThenElseCommand n ) {
		'''
		if («n.condition.expressionToJava») «n.thenBlock.vertexInstantiation»
		«IF n.elseBlock != null»else «n.elseBlock.vertexInstantiation»«ENDIF»
		'''
	}
	
	def dispatch CharSequence vertexInstantiation( NodeBlockCommand block ) {
		''' 
		{
			«FOR n:block.nodes»
			«n.vertexInstantiation»
			«ENDFOR»
		}
		'''
	}
	
	def dispatch CharSequence vertexInstantiation( NodeForCommand f ) {
		''' 
		for( int «f.variable.name.variableName» = «IF f.start==null»0«ELSE»«f.start.expressionToJava»«ENDIF» ; «f.variable.name.variableName» < «f.end.expressionToJava» ; «f.variable.name.variableName» += «IF f.step==null»1«ELSE»«f.step.expressionToJava»«ENDIF» ) 
			«f.body.vertexInstantiation»
		'''
	}
	
	def dispatch CharSequence vertexInstantiation( NodeForEach f ) {
		var eType = f.iteration.value.typeOf
		if ((!eType.isList)&&(!eType.isSet)) {
			'''
			//ERROR!!!
			'''
		} else {
			'''
			for( «f.iteration.typeOf.toJavaType(false)» «f.iteration.name.variableName» : «f.iteration.value.expressionToJava» ) 
				«f.body.vertexInstantiation»
			'''
		}
	}
	
	def dispatch CharSequence generateEdgeCode( ConnectionDeclaration e ) {
		'''
		{
			Node l1 = «e.source.vertexCode»;
			Node l2 = «e.target.vertexCode»;
			if ((l1 != null)&&(l2 != null)) {
				HashMap<String,Object> data = new HashMap<>();
				«FOR ep:e.edgeProperties»
				data.put( "«ep.name»" , «ep.value.expressionToJava» );
				«ENDFOR»
				sm.addEdge( l1 , data , l2);
				«IF e.direction instanceof UnDirectedEdge»	
				sm.addEdge( l2, data , l1 );
				«ENDIF»			
			}
		}		
		'''		
	}

	def dispatch CharSequence generateEdgeCode( ConnectionIfThenElseCommand e ) {
		'''
		if («e.condition.expressionToJava») «e.thenBlock.generateEdgeCode»
		«IF e.elseBlock != null»else «e.elseBlock.generateEdgeCode»«ENDIF»
		'''
	}

	def dispatch CharSequence generateEdgeCode( ConnectionForCommand e ) {
		''' 
		for( int «e.variable.name.variableName» = «IF e.start==null»0«ELSE»«e.start.expressionToJava»«ENDIF» ; «e.variable.name.variableName» < «e.end.expressionToJava» ; «e.variable.name.variableName» += «IF e.step==null»1«ELSE»«e.step.expressionToJava»«ENDIF» ) 
			«e.body.generateEdgeCode»
		'''
	}

	def dispatch CharSequence generateEdgeCode( ConnectionForEach e ) {
		var eType = e.iteration.value.typeOf
		if ((!eType.isList)&&(!eType.isSet)) {
			'''
			//ERROR!!!
			'''
		} else {
			'''
			for( «e.iteration.typeOf.toJavaType(false)» «e.iteration.name.variableName»:  «e.iteration.value.expressionToJava» ) 
				«e.body.generateEdgeCode»
			'''
		}
	}

	def dispatch CharSequence generateEdgeCode( ConnectionBlockCommand e ) {
		'''
		{
			«FOR c:e.edges»
			«c.generateEdgeCode»
			«ENDFOR»
		}
		'''
	}

	def dispatch CharSequence generateAreaCode( AreaElementDeclaration d ) {
		'''
		tmpNode = «d.node.vertexCode»;
		if (tmpNode != null) {
			area.add( tmpNode );
		}
		'''
	}
	
	def dispatch CharSequence generateAreaCode( AreaIfThenElseCommand n ) {
		'''
		if («n.condition.expressionToJava») «n.thenBlock.generateAreaCode»
		«IF n.elseBlock != null»else «n.elseBlock.generateAreaCode»«ENDIF»
		'''
	}
	
	def dispatch CharSequence generateAreaCode( AreaBlockCommand block ) {
		''' 
		{
			«FOR n:block.nodes»
			«n.generateAreaCode»
			«ENDFOR»
		}
		'''
	}
	
	def dispatch CharSequence generateAreaCode( AreaForCommand f ) {
		''' 
		for( int «f.variable.name.variableName» = «IF f.start==null»0«ELSE»«f.start.expressionToJava»«ENDIF» ; «f.variable.name.variableName» < «f.end.expressionToJava» ; «f.variable.name.variableName» += «IF f.step==null»1«ELSE»«f.step.expressionToJava»«ENDIF» ) 
			«f.body.generateAreaCode»
		'''
	}
	
	def dispatch CharSequence generateAreaCode( AreaForEach f ) {
		var eType = f.iteration.typeOf
		if ((!eType.isList)&&(!eType.isSet)) {
			'''
			//ERROR!!!
			'''
		} else {
			'''
			for( «eType.toJavaType(false)» «f.iteration.name.variableName»:  «f.iteration.value.expressionToJava» ) 
				«f.body.generateAreaCode»
			'''
		}
	}
	
//	def dispatch generateEdgeExpression( EList<UniverseElement> universe , AtomicConnectionExpression e ) {
//		//FIXME!!!!
//		var vars = e.source.elements.filter(typeof(LocationVariable)).map[ it -> e.source.elements.indexOf( it ) ]
//		var values = e.source.elements.filter(typeof(LocationExpressionValue)).map[ it -> e.source.elements.indexOf( it ) ]
//		'''
//		{
//			«FOR v:vars»
//			«IF v.value<universe.size»
//			«universe.get(v.value).type.toJavaType» «v.key.name.variableName»  = («universe.get(v.value).type.toJavaType») l.get( «v.value» );
//			«ENDIF»
//			«ENDFOR»
//			«IF !values.empty»
//			if (
//			«FOR v:values SEPARATOR "&&"»
//				((«v.key.value.expressionToJava»)==((«universe.get(v.value).typeOf.toJavaType(true)») l.get( «e.source.elements.indexOf(v)» )))
//			«ENDFOR»
//			) {
//			«ENDIF»
//				if («e.guard ?. expressionToJava ?: "true"») {
//					Node l2 = «e.trg.vertexCode»;
//					HashMap<String,Object> data = new HashMap<>();
//					«FOR ep:e.edgeProperties»
//					data.put( "«ep.name»" , «ep.value.expressionToJava» );
//					«ENDFOR»
//					sm.addEdge( l , data , l2);
//					«IF e.direction instanceof UnDirectedEdge»	
//					sm.addEdge( l2, data , l );
//					«ENDIF»			
//				}
//			«IF !values.empty»
//			}
//			«ENDIF»
//		}
//		'''
//		''''''
//	} 
	
	def dispatch vertexCode( NamedLocationExpression e ) {
		'''sm.getVertex( "«e.ref.name»" , new Tuple( «FOR v:e.values SEPARATOR ','»«v.expressionToJava»«ENDFOR») )'''
	}
	
	def dispatch vertexCode( UnNamedLocationExpression e ) {
		'''sm.getVertex( new Tuple( «FOR v:e.values SEPARATOR ','»«v.expressionToJava»«ENDFOR») )'''
	}
	

	
	
	
}