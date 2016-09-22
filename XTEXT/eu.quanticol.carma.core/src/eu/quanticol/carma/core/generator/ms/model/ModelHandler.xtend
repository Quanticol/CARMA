package eu.quanticol.carma.core.generator.ms.model

import eu.quanticol.carma.core.carma.Model
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.ms.enums.EnumHandler
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.generator.ms.record.RecordHandler
import eu.quanticol.carma.core.generator.ms.activities.ActivityHandler
import eu.quanticol.carma.core.generator.ms.function.FunctionHandler
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.system.SystemHandler
import eu.quanticol.carma.core.generator.ms.measure.MeasureHandler
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.typing.TypeSystem
import eu.quanticol.carma.simulator.space.SpaceModel
import eu.quanticol.carma.core.carma.SpaceDefinition
import org.eclipse.emf.common.util.EList
import eu.quanticol.carma.core.carma.LocationVariable
import eu.quanticol.carma.core.carma.LocationExpressionValue
import eu.quanticol.carma.core.carma.UnDirectedEdge
import eu.quanticol.carma.core.carma.LabelDefinition
import eu.quanticol.carma.core.carma.NodeDeclaration
import eu.quanticol.carma.core.carma.NamedNode
import eu.quanticol.carma.core.carma.UnNamedNode
import eu.quanticol.carma.core.carma.NodeIfThenElseCommand
import eu.quanticol.carma.core.carma.NodeBlockCommand
import eu.quanticol.carma.core.carma.NodeForCommand
import eu.quanticol.carma.core.carma.NodeForEach
import eu.quanticol.carma.core.carma.ConnectionExpression
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.UniverseElement
import eu.quanticol.carma.core.carma.NamedLocationExpression
import eu.quanticol.carma.core.carma.UnNamedLocationExpression
import eu.quanticol.carma.core.carma.DirectedEdge

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
		import eu.quanticol.carma.simulator.space.SpaceModel.Node;
		import eu.quanticol.carma.simulator.space.SpaceModel;
		import eu.quanticol.carma.simulator.space.Tuple;
		import java.util.LinkedList;
		import java.util.HashSet;
		import java.util.HashMap;
		import java.util.Map;
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
			
			public static boolean carmaEquals( Object o1 , Object o2 ) {
				if (o1 == o2) {
					return true;				 
				}	
				if ((o1 == null)||(o2==null)) {
					return false;	
				}
				return o1.equals( o2 );
			}
			
			public static <T> LinkedList<T> getList( T ... elements ) {
				LinkedList<T> foo = new LinkedList<T>();
				for (int i=0 ; i<elements.length ; i++ ) {
					foo.add(elements[i]);	
				}	
				return foo;
			}
			
			public static <T> T get( LinkedList<T> list , int idx ) {
				if (list == null) {
					return null;	
				}	
				if ((idx<0)||(idx>=list.size())) {
					return null;
				}
				return list.get( idx );
			}
			
			public static <T> HashSet<T> getSet( T ... elements ) {
				HashSet<T> foo = new HashSet<T>();
				for (int i=0 ; i<elements.length ; i++ ) {
					foo.add(elements[i]);	
				}	
				return foo;
			}
			
			public static <T> LinkedList<T> concatenate( LinkedList<T> l1 , LinkedList<T> l2 ) {
				LinkedList<T> result = new LinkedList<T>();
				result.addAll(l1);
				result.addAll(l2);
				return result;
			}

			public static <T> HashSet<T> union( HashSet<T> s1 , HashSet<T> s2 ) {
				HashSet<T> result = new HashSet<T>();
				result.addAll(s1);
				result.addAll(s2);
				return result;
			}

			public static <T> HashSet<T> intersection( HashSet<T> s1 , HashSet<T> s2 ) {
				HashSet<T> result = new HashSet<T>();
				result.addAll(s1);
				result.retainAll(s2);
				return result;
			}

			public static <T> HashSet<T> removeAll( HashSet<T> s1 , HashSet<T> s2 ) {
				HashSet<T> result = new HashSet<T>();
				result.addAll(s1);
				result.removeAll(s2);
				return result;
			}

			public static <T> LinkedList<T> removeAll( LinkedList<T> s1 , LinkedList<T> s2 ) {
				LinkedList<T> result = new LinkedList<T>();
				result.addAll(s1);
				result.removeAll(s2);
				return result;
			}
			
			public static <T,R> LinkedList<R> map( LinkedList<T> s1 , Function<T, R> f ) {
				LinkedList<R> result = new LinkedList<R>();
				for (T v: s1) {
					result.add( f.apply( v ) );	
				}	
				return result;
			}

			public static <T> LinkedList<T> filter( LinkedList<T> s1 , Predicate<T> f ) {
				LinkedList<T> result = new LinkedList<T>();
				for (T v: s1) {
					if (f.test( v )) {
						result.add( v );	
					}
				}	
				return result;
			}

			public static <T,R> HashSet<R> map( HashSet<T> s1 , Function<T, R> f ) {
				HashSet<R> result = new HashSet<R>();
				for (T v: s1) {
					result.add( f.apply( v ) );	
				}	
				return result;
			}
			
			public static <T> HashSet<T> filter( HashSet<T> s1 , Predicate<T> f ) {
				HashSet<T> result = new HashSet<T>();
				for (T v: s1) {
					if (f.test( v )) {
						result.add( v );	
					}
				}	
				return result;
			}
			
			public static <T> T pick( HashSet<T> s1 ) {
				return pick( s1 , x -> true );			
			}
			
			public static <T> T pick( HashSet<T> s1 , Predicate<T> f ) {
				T result = null;
				for (T v: s1) {
					if ((result==null)&&(f.test(v))) {
						result = v;	
					}
				}
				if (result != null) {
					s1.remove( result );	
				}
				return result;
			}

			public static <T> T pick( LinkedList<T> s1 , Predicate<T> f ) {
				for (int i=0; i<s1.size() ; i++) {
					T v = s1.get( i );
					if (f.test(v)) {
						s1.remove( i );
						return v;	
					}
				}
				return null;
			}
			
			public static <T> boolean exist( Collection<T> c , Predicate<T> f ) {
				for (T v: c) {
					if (f.test(v)) {
						return true;	
					}		
				}
				return false;
			}
			
			public static <T> boolean forall( Collection<T> c , Predicate<T> f ) {
				for (T v: c) {
					if (!f.test(v)) {
						return false;	
					}		
				}
				return true;
			}
			
			public static <T> T head( LinkedList<T> l ) {
				return l.peekFirst();
			}
			
			public static <T> int computeSize( Collection<T> l ) {
				return l.size();	
			}
			
			public static LinkedList<Integer> generateIntervalList( int min , int max ) {
				LinkedList<Integer> toReturn = new LinkedList<>();
				for( int i=min ; i<max; i++ ) {
					toReturn.add( i );	
				}
				return toReturn;
			}

			«FOR e:model.enums» 
			«e.enumToJava»
			«ENDFOR»
			
			«FOR r:model.records»
			«r.recordToJava»
			«ENDFOR»
		
			«FOR c:model.constants»
			public static final «c.value.typeOf.toJavaType(true)» «c.name.constantName» = «c.value.expressionToJava»;
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
			
			for( Node l: sm.getAll() ) {
				«FOR e:s.edges»
				«s.universe.generateEdgeExpression(e)»
				«ENDFOR»
			}
		
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
		var eType = f.iteration.typeOf
		if (eType.isNone) {
			'''
			//ERROR!!!
			'''
		} else {
			'''
			for( «eType.toJavaType(false)» «f.iteration.name.variableName»:  «f.iteration.value.expressionToJava» ) 
				«f.body.vertexInstantiation»
			'''
		}
	}
	

	
	def generateEdgeExpression( EList<UniverseElement> universe , ConnectionExpression e ) {
		var vars = e.source.elements.filter(typeof(LocationVariable)).map[ it -> e.source.elements.indexOf( it ) ]
		var values = e.source.elements.filter(typeof(LocationExpressionValue)).map[ it -> e.source.elements.indexOf( it ) ]
		'''
		{
			«FOR v:vars»
			«IF v.value<universe.size»
			«universe.get(v.value).type.toJavaType» «v.key.name.variableName»  = («universe.get(v.value).type.toJavaType») l.get( «v.value» );
			«ENDIF»
			«ENDFOR»
			«IF !values.empty»
			if (
			«FOR v:values SEPARATOR "&&"»
				((«v.key.value.expressionToJava»)==((«universe.get(v.value).typeOf.toJavaType(true)») l.get( «e.source.elements.indexOf(v)» )))
			«ENDFOR»
			) {
			«ENDIF»
				if («e.guard ?. expressionToJava ?: "true"») {
					Node l2 = «e.trg.vertexCode»;
					HashMap<String,Object> data = new HashMap<>();
					«FOR ep:e.edgeProperties»
					data.put( "«ep.name»" , «ep.value.expressionToJava» );
					«ENDFOR»
					sm.addEdge( l , data , l2);
					«IF e.direction instanceof UnDirectedEdge»	
					sm.addEdge( l2, data , l );
					«ENDIF»			
				}
			«IF !values.empty»
			}
			«ENDIF»
		}
		'''
	} 
	
	def dispatch vertexCode( NamedLocationExpression e ) {
		'''sm.getVertex( "«e.ref.name»" , new Tuple( «FOR v:e.values SEPARATOR ','»«v.expressionToJava»«ENDFOR») )'''
	}
	
	def dispatch vertexCode( UnNamedLocationExpression e ) {
		'''sm.getVertex( new Tuple( «FOR v:e.values SEPARATOR ','»«v.expressionToJava»«ENDFOR») )'''
	}
	

	
	
	
}