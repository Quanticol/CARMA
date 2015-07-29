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
import static extension eu.quanticol.carma.core.utils.Util.*
import eu.quanticol.carma.core.carma.FieldAssignment
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.IfThenElseExpression
import eu.quanticol.carma.core.carma.Reference

class ExpressionHandler {
	
	def static dispatch CharSequence expressionToJava( Or e ) {
		'''( «e.left.expressionToJava» )||( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( And e ) {
		'''( «e.left.expressionToJava» )&&( «e.right.expressionToJava» )'''
	}

	def static dispatch CharSequence expressionToJava( Equality e ) {
		'''( «e.left.expressionToJava» )==( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( DisEquality e ) {
		'''( «e.left.expressionToJava» )!=( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Less e ) {
		'''( «e.left.expressionToJava» )<( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( LessOrEqual e ) {
		'''( «e.left.expressionToJava» )<=( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Greater e ) {
		'''( «e.left.expressionToJava» )>( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( GreaterOrEqual e ) {
		'''( «e.left.expressionToJava» )>=( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Subtraction e ) {
		'''( «e.left.expressionToJava» )-( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Addition e ) {
		'''( «e.left.expressionToJava» )+( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Division e ) {
		'''( «e.left.expressionToJava» )/( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Multiplication e ) {
		'''( «e.left.expressionToJava» )*( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Modulo e ) {
		'''( «e.left.expressionToJava» )%( «e.right.expressionToJava» )'''
	}
	
	def static dispatch CharSequence expressionToJava( Not e ) {
		'''!( «e.expression.expressionToJava» )'''
	}

	def static dispatch CharSequence expressionToJava( UnaryPlus e ) {
		'''«e.expression.expressionToJava»'''
	}

	def static dispatch CharSequence expressionToJava( UnaryMinus e ) {
		'''-(«e.expression.expressionToJava»)'''
	}
	
	def static dispatch CharSequence expressionToJava( Reference e ) {
		'''«e.reference.getReference( ReferenceContext::NONE )»«IF e.isIsCall»( 
			«FOR p:e.args SEPARATOR ','»«p.expressionToJava»«ENDFOR»
		)«ENDIF»'''		
	}

	def static dispatch CharSequence expressionToJava( RecordAccess e ) {
		'''«e.source.expressionToJava».«e.field.name.fieldName»'''		
	}
		
	def static dispatch CharSequence expressionToJava( IfThenElseExpression e ) {
		'''( «e.guard.expressionToJava» ? «e.thenBranch.expressionToJava» ; «e.elseBranch.expressionToJava» )'''		
	}

	
	def static dispatch CharSequence expressionToJava( AtomicTrue e ) {
		'''true'''		
	}
	
	def static dispatch CharSequence expressionToJava( AtomicFalse e ) {
		'''false'''		
	}

	def static dispatch CharSequence expressionToJava( AtomicInteger e ) {
		'''«e.value»'''
	}

	def static dispatch CharSequence expressionToJava( AtomicReal e ) {
		'''«e.value»'''
	}

	def static dispatch CharSequence expressionToJava( AtomicRecord e ) {
		var record = e.fields.head.field.getContainerOfType(typeof(RecordDefinition))
		if (record == null) {
			'''null'''
		} else {
			var orderedArgs = record.fields.map[ field | e.fields.findFirst[ it.field==field ]]
			'''new «record.name.recordClass»( «FOR fa:orderedArgs SEPARATOR ','»«fa.value.expressionToJava»«ENDFOR»)'''						
		}		
	}	


	def static dispatch CharSequence expressionToJava( AtomicNow e ) {
		'''now'''
	}

	def static dispatch CharSequence expressionToJava( SetComp	e ) {
		//FIXME!!!!!		
	}

	def static dispatch CharSequence expressionToJava( AtomicPi e ) {
		'''Math.PI'''
	}

	def static dispatch CharSequence expressionToJava( AtomicExp e ) {
		'''Math.E'''
	}

	def static dispatch getLitteral( Reference s , ReferenceContext c ) {
		'''
		«s.reference.getReference(c)»«IF s.isIsCall»(
			«FOR p:s.args SEPARATOR ','»«p.expressionToJava»«ENDFOR»
		)«ENDIF»
		''' 
	}

	def static dispatch CharSequence expressionToJava( MyContext e ) {
		e.reference.getLitteral( ReferenceContext::MY )
	}

	def static dispatch CharSequence expressionToJava( ReceiverContext e ) {
		e.reference.getLitteral( ReferenceContext::RECEIVER )
	}

	def static dispatch CharSequence expressionToJava( SenderContext e ) {
		e.reference.getLitteral( ReferenceContext::SENDER )
	}

	def static dispatch CharSequence expressionToJava( GlobalContext e ) {
		e.reference.getLitteral( ReferenceContext::GLOBAL )
	} 
	
	def static dispatch CharSequence expressionToJava( AbsFunction e ) {
		'''Math.abs( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( AcosFunction e ) {
		'''Math.acos( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( AsinFunction e ) {
		'''Math.asin( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( AtanFunction e ) {
		'''Math.atan( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( Atan2Function e ) {
		'''Math.atan2( «e.first.expressionToJava» , «e.second.expressionToJava» )'''		
	} 	
	
	def static dispatch CharSequence expressionToJava( CbrtFunction e ) {
		'''Math.cbrt( «e.arg.expressionToJava» )'''		
	} 	
	
	def static dispatch CharSequence expressionToJava( CeilFunction e ) {
		'''Math.ceil( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( CosFunction e ) {
		'''Math.cos( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( ExpFunction e ) {
		'''Math.exp( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( FloorFunction e ) {
		'''Math.floor( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( LogFunction e ) {
		'''Math.log( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( Log10Function e ) {
		'''Math.log10( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( MaxFunction e ) {
		'''Math.max( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( MinFunction e ) {
		'''Math.min( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( PowFunction e ) {	
		'''Math.pow( «e.first.expressionToJava» , «e.second.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( SinFunction e ) {
		'''Math.sin( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( SqrtFunction e ) {
		'''Math.sqrt( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( TanFunction e ) {
		'''Math.tan( «e.arg.expressionToJava» )'''	
	} 	
	
	def static dispatch CharSequence expressionToJava( UniformFunction e ) {
		//FIXME!!!
	}
}