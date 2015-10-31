package eu.quanticol.carma.core.generator.ms.expression

import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.DisEquality
import eu.quanticol.carma.core.carma.Less
import eu.quanticol.carma.core.carma.LessOrEqual
import eu.quanticol.carma.core.carma.Greater
import eu.quanticol.carma.core.carma.GreaterOrEqual
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.UnaryPlus
import eu.quanticol.carma.core.carma.UnaryMinus
import eu.quanticol.carma.core.carma.RecordAccess
import eu.quanticol.carma.core.carma.AtomicTrue
import eu.quanticol.carma.core.carma.AtomicFalse
import eu.quanticol.carma.core.carma.AtomicInteger
import eu.quanticol.carma.core.carma.AtomicReal
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.AtomicPi
import eu.quanticol.carma.core.carma.AtomicExp
import eu.quanticol.carma.core.carma.MyContext
import eu.quanticol.carma.core.carma.ReceiverContext
import eu.quanticol.carma.core.carma.SenderContext
import eu.quanticol.carma.core.carma.GlobalContext
import eu.quanticol.carma.core.carma.AbsFunction
import eu.quanticol.carma.core.carma.AcosFunction
import eu.quanticol.carma.core.carma.AsinFunction
import eu.quanticol.carma.core.carma.AtanFunction
import eu.quanticol.carma.core.carma.Atan2Function
import eu.quanticol.carma.core.carma.CbrtFunction
import eu.quanticol.carma.core.carma.CeilFunction
import eu.quanticol.carma.core.carma.CosFunction
import eu.quanticol.carma.core.carma.ExpFunction
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.LogFunction
import eu.quanticol.carma.core.carma.Log10Function
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.PowFunction
import eu.quanticol.carma.core.carma.SinFunction
import eu.quanticol.carma.core.carma.SqrtFunction
import eu.quanticol.carma.core.carma.TanFunction
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.utils.ReferenceContext

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.IfThenElseExpression
import eu.quanticol.carma.core.carma.Reference
import eu.quanticol.carma.core.utils.Util
import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AllComponents
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.AComponentAllStates
import eu.quanticol.carma.core.carma.AComponentAState
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.carma.MaxMeasure
import eu.quanticol.carma.core.carma.MinMeasure
import eu.quanticol.carma.core.carma.AverageMeasure
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.RealType
import eu.quanticol.carma.core.carma.IntegerType
import eu.quanticol.carma.core.carma.ReferenceableElement
import eu.quanticol.carma.core.carma.FunctionDefinition
import java.util.List
import eu.quanticol.carma.core.carma.ValueType
import eu.quanticol.carma.core.carma.AtomicRnd
import eu.quanticol.carma.core.carma.CastToReal
import eu.quanticol.carma.core.carma.CastToInteger
import eu.quanticol.carma.core.typing.TypeSystem

class ExpressionHandler {
	
	@Inject extension Util
	@Inject extension AttributeHandler
	@Inject extension TypeSystem
	
	String currentComponent = ""
	
	def setCurrentComponent( String current ) {
		currentComponent = current;
	}
	
	def dispatch CharSequence expressionToJava( CastToReal e ) {
		'''(double) ( «e.arg.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( CastToInteger e ) {
		'''(int) ( «e.arg.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Or e ) {
		'''( «e.left.expressionToJava» )||( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( And e ) {
		'''( «e.left.expressionToJava» )&&( «e.right.expressionToJava» )'''
	}

	def dispatch CharSequence expressionToJava( Equality e ) {
		if (e.left.typeOf.isRecord) {
			'''( «e.left.expressionToJava» ).equals( «e.right.expressionToJava» )'''
		} else {
			'''( «e.left.expressionToJava» )==( «e.right.expressionToJava» )'''
		}
	}
	
	def dispatch CharSequence expressionToJava( DisEquality e ) {
		'''( «e.left.expressionToJava» )!=( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Less e ) {
		'''( «e.left.expressionToJava» )<( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( LessOrEqual e ) {
		'''( «e.left.expressionToJava» )<=( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Greater e ) {
		'''( «e.left.expressionToJava» )>( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( GreaterOrEqual e ) {
		'''( «e.left.expressionToJava» )>=( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Subtraction e ) {
		'''( «e.left.expressionToJava» )-( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Addition e ) {
		'''( «e.left.expressionToJava» )+( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Division e ) {
		'''( «e.left.expressionToJava» )/( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Multiplication e ) {
		'''( «e.left.expressionToJava» )*( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Modulo e ) {
		'''( «e.left.expressionToJava» )%( «e.right.expressionToJava» )'''
	}
	
	def dispatch CharSequence expressionToJava( Not e ) {
		'''!( «e.expression.expressionToJava» )'''
	}

	def dispatch CharSequence expressionToJava( UnaryPlus e ) {
		'''«e.expression.expressionToJava»'''
	}

	def dispatch CharSequence expressionToJava( UnaryMinus e ) {
		'''-(«e.expression.expressionToJava»)'''
	}
	
	def dispatch CharSequence expressionToJava( Reference e ) {
		'''«e.reference.getReference( ReferenceContext::NONE , currentComponent  )»«IF e.isIsCall»( 
			«e.args.map[it.expressionToJava].invocationParameters( e.reference.functionArguments.map[it.type] )»
		)«ENDIF»«IF e.typeOf.isRecord».clone()«ENDIF»'''		
	}

	def getFunctionArguments( ReferenceableElement e ) {
		switch e {
			FunctionDefinition: e.parameters
			default: newLinkedList()
		}
	}	

	def dispatch CharSequence expressionToJava( RecordAccess e ) {
		'''«e.source.expressionToJava».«e.field.name.fieldName»'''		
	}
		
	def dispatch CharSequence expressionToJava( IfThenElseExpression e ) {
		'''( «e.guard.expressionToJava» ? «e.thenBranch.expressionToJava» : «e.elseBranch.expressionToJava» )'''		
	}

	
	def dispatch CharSequence expressionToJava( AtomicTrue e ) {
		'''true'''		
	}
	
	def dispatch CharSequence expressionToJava( AtomicFalse e ) {
		'''false'''		
	}

	def dispatch CharSequence expressionToJava( AtomicInteger e ) {
		'''«e.value»'''
	}

	def dispatch CharSequence expressionToJava( AtomicReal e ) {
		'''«e.value»'''
	}

	def dispatch CharSequence expressionToJava( AtomicRecord e ) {
		var record = e.fields.head.field.getContainerOfType(typeof(RecordDefinition))
		if (record == null) {
			'''null'''
		} else {
			var orderedArgs = record.fields.map[ field | e.fields.findFirst[ it.field==field ]].map[ it.value ]
			'''new «record.name.recordClass»( «orderedArgs.map[it.expressionToJava].invocationParameters( record.fields.map[ it.fieldType ] )» )'''
		}		
	}	


	def dispatch CharSequence expressionToJava( AtomicNow e ) {
		'''now'''
	}

	def dispatch CharSequence expressionToJava( AtomicRnd e ) {
		'''RandomGeneratorRegistry.rnd()'''
	}


	def dispatch CharSequence expressionToJava( SetComp	e ) {
		'''
		system.measure( 
			«e.variable.getComponentPredicate(e.predicate)»
		)
		'''
	}
	
	def dispatch CharSequence expressionToJava( MaxMeasure	e ) {
		'''
		system.max( 
			new Measure<CarmaStore>() {

				public double measure(CarmaStore store) {
					«FOR a:e.globalAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
					«ENDFOR»
					«FOR a:e.myAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
					«ENDFOR»
					«FOR a:e.referencedAttibutes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
					«ENDFOR»
					return «e.value.expressionToJava»;
				}
			
				public String getName() {
					return "ANONYMOUS MEASURE";
				}
				
			} , 
			«e.guard.predicateOfAnExpression»
		)
		'''
	}
	
	def dispatch CharSequence expressionToJava( MinMeasure	e ) {
		'''
		system.min( 
			new Measure<CarmaStore>() {

				public double measure(CarmaStore store) {
					«FOR a:e.globalAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
					«ENDFOR»
					«FOR a:e.myAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
					«ENDFOR»
					«FOR a:e.referencedAttibutes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
					«ENDFOR»
					return «e.value.expressionToJava»;
				}
			
				public String getName() {
					return "ANONYMOUS MEASURE";
				}
				
			} , 
			«e.guard.predicateOfAnExpression»
		)
		'''
	}
	
	
	def dispatch CharSequence expressionToJava( AverageMeasure	e ) {
		'''
		system.average( 
			Measure<CarmaStore> m2 = new Measure<CarmaStore>() {

				public double measure(CarmaStore store) {
					«FOR a:e.globalAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
					«ENDFOR»
					«FOR a:e.senderAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
					«ENDFOR»
					«FOR a:e.myAttributes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
					«ENDFOR»
					«FOR a:e.referencedAttibutes»
					«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
					«ENDFOR»
					return «e.value.expressionToJava»;
				}
			
				public String getName() {
					return "ANONYMOUS MEASURE";
				}
				
			} , 
			«e.guard.predicateOfAnExpression»
		)
		'''
	}	
	
	def dispatch getComponentPredicate( AllComponents p , Expression e ) {
		'''
		new BasicComponentPredicate(
			«e.predicateOfAnExpression»
			«IF p.states != null»
			«FOR n:p.states.stateNames»
			, new CarmaProcessPredicate() {

				//@Override
				public boolean eval(CarmaProcess p) {
					if (p instanceof CarmaSequentialProcess) {
						CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
						try{
							return csp.getState().getName().equals("«n»");
						} catch (NullPointerException e) {
							e.printStackTrace();
							return false;
						}
					}
					return false;
				}
							
			}
			«ENDFOR» 			
			«ENDIF»
		)
		'''		
	}

	def dispatch getComponentPredicate( AComponentAllStates p , Expression e ) {
		'''
		new BasicComponentPredicate(
			«e.predicateOfAnExpression» , 
			new CarmaProcessPredicate() {

				//@Override
				public boolean eval(CarmaProcess p) {
					if (p instanceof CarmaSequentialProcess) {
						CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
						try{
							return csp.getName().equals("«p.comp.name»");
						} catch (NullPointerException e) {
							e.printStackTrace();
							return false;
						}	
					}
					return false;
				}
							
			}
			)
		'''		
	}

	def dispatch getComponentPredicate( AComponentAState p , Expression e ) {
		'''
		new BasicComponentPredicate(
			«e.predicateOfAnExpression»
			«FOR n:p.state.stateNames»
			, new CarmaProcessPredicate() {

				//@Override
				public boolean eval(CarmaProcess p) {
					if (p instanceof CarmaSequentialProcess) {
						CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
						try{
							return csp.getName().equals("«p.comp.name»")&&csp.getState().getName().equals("«n»");
						} catch (NullPointerException e) {
							return false;
						}
					}
					return false;
				}
							
			}
			«ENDFOR» 
			)
		'''
	}
	
	def predicateOfAnExpression( Expression e ) {
		'''
		«IF e != null»
		new CarmaPredicate() {
			
			//Here we assume that the following "final" references are available (if needed):
			//- global: reference to the global store;
			//- sender: reference to the store of sender;
			//- receiver: reference to the store of the receiver;				
			//@Override
			public boolean satisfy(CarmaStore store) {
				«FOR a:e.globalAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
				«ENDFOR»
				«FOR a:e.senderAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
				«ENDFOR»
				«FOR a:e.receiverAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
				«ENDFOR»
				«FOR a:e.myAttributes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::MY,"store")»
				«ENDFOR»
				«FOR a:e.referencedAttibutes»
				«a.attributeTemporaryVariableDeclaration(ReferenceContext::NONE,"store")»
				«ENDFOR»
				try{
					return «e.expressionToJava»;
				} catch (NullPointerException e) {
					e.printStackTrace();
					return false;
				}
			}
		
			
		}
		«ELSE»
		CarmaPredicate.TRUE
		«ENDIF»
		'''
	}

	def dispatch Iterable<String> stateNames( ProcessReference p ) {
		if (p.expression instanceof ProcessState) {
			newLinkedList(p.expression.name)
		} else {
			newLinkedList()				
		}
	}
	
	def dispatch Iterable<String> stateNames( ParallelComposition p ) {
		p.left.stateNames+p.right.stateNames
	}
	

	def dispatch CharSequence expressionToJava( AtomicPi e ) {
		'''Math.PI'''
	}

	def dispatch CharSequence expressionToJava( AtomicExp e ) {
		'''Math.E'''
	}

	def getLitteral( Reference s , ReferenceContext c ) {
		'''
		«s.reference.getReference(c,currentComponent)»«IF s.isIsCall»(
			«FOR p:s.args SEPARATOR ','»«p.expressionToJava»«ENDFOR»
		)«ENDIF»
		''' 
	}

	def dispatch CharSequence expressionToJava( MyContext e ) {
		e.reference.getReference( ReferenceContext::MY , currentComponent )
	}

	def dispatch CharSequence expressionToJava( ReceiverContext e ) {
		e.reference.getReference( ReferenceContext::RECEIVER , currentComponent )
	}

	def dispatch CharSequence expressionToJava( SenderContext e ) {
		e.reference.getReference( ReferenceContext::SENDER , currentComponent )
	}

	def dispatch CharSequence expressionToJava( GlobalContext e ) {
		e.reference.getReference( ReferenceContext::GLOBAL , currentComponent )
	} 
	
	def dispatch CharSequence expressionToJava( AbsFunction e ) {
		'''Math.abs( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( AcosFunction e ) {
		'''Math.acos( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( AsinFunction e ) {
		'''Math.asin( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( AtanFunction e ) {
		'''Math.atan( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( Atan2Function e ) {
		'''Math.atan2( «e.first.expressionToJava» , «e.second.expressionToJava» )'''		
	} 	
	
	def dispatch CharSequence expressionToJava( CbrtFunction e ) {
		'''Math.cbrt( «e.arg.expressionToJava» )'''		
	} 	
	
	def dispatch CharSequence expressionToJava( CeilFunction e ) {
		'''Math.ceil( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( CosFunction e ) {
		'''Math.cos( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( ExpFunction e ) {
		'''Math.exp( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( FloorFunction e ) {
		'''Math.floor( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( LogFunction e ) {
		'''Math.log( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( Log10Function e ) {
		'''Math.log10( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( MaxFunction e ) {
		'''Math.max( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( MinFunction e ) {
		'''Math.min( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( PowFunction e ) {	
		'''Math.pow( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( SinFunction e ) {
		'''Math.sin( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( SqrtFunction e ) {
		'''Math.sqrt( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( TanFunction e ) {
		'''Math.tan( «e.arg.expressionToJava» )'''	
	} 	
	
	def dispatch CharSequence expressionToJava( UniformFunction e ) {
		'''RandomGeneratorRegistry.uniform(«FOR v:e.args SEPARATOR ','»«v.expressionToJava»«ENDFOR»)'''
	}
	
	def invocationParameters( List<CharSequence> args , List<ValueType> parms ) {
		var indexedArgs = args.indexed
		'''
		«FOR ip:indexedArgs SEPARATOR ','»
		«ip.value.getParameterCode(parms.get(ip.key))»
		«ENDFOR»
		'''
	}
	
	def getParameterCode( CharSequence code , ValueType vt ) {
		switch vt {
			RealType: '''Double.valueOf( «code» )'''
			IntegerType: '''Integer.valueOf( «code» )'''
			default: code		
		}
	}
}