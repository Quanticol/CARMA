package eu.quanticol.carma.tools.fluid

import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.ProcessExpression
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionNil
import eu.quanticol.carma.core.carma.ProcessExpressionKill
import eu.quanticol.carma.core.carma.ProcessExpressionReference

class ProcessDerivative {
	
	Iterable<ProcessState> derivative
	String repr
	int hashCode
	
	new( ProcessState state ) {
		derivative = newLinkedList( state )
	}

	new( Iterable<ProcessState> derivative ) {
		this.derivative = derivative.sortWith( [ x,y | x.name.compareTo( y.name ) ] )
	}
	
	
	new( ProcessDerivative p1 , ProcessDerivative p2 ) {
		derivative = ( p1.derivative + p2.derivative ).sortWith( [ x,y | x.name.compareTo( y.name ) ] )
	}
	
	override equals( Object o ) {
		if (o instanceof ProcessDerivative) {
			derivative.elementsEqual( o.derivative )			
		} else {
			true
		}
	}
	
	private def memoize( ) {
		repr = derivative.map[ it.name ].join("|")			
		hashCode = repr.hashCode
	}
	
	override toString( ) {
		if (repr == null) {
			memoize()
		} 
		repr			
	}
	
	override hashCode() {
		if (repr == null) {
			memoize()
		}
		hashCode			
	}
	
	def transitions( ) {
		var next = derivative.map[ it.processExpression.transition( newLinkedList() ) ]
		next.indexed.map[
			val idx = it.key
			it.value.map[
				it.combine( this.remove( idx ) )
			]
		].flatten
	}
	
	def remove( int idx ) {		
		new ProcessDerivative( derivative.indexed.filter[it.key != idx].map[it.value] ) 
	}
	
	def dispatch Iterable<Transition> transition( ProcessExpression pexp , Iterable<Expression> guards ) {
		newLinkedList()				
	}
	
	def dispatch Iterable<Transition> transition( ProcessExpressionChoice pexp , Iterable<Expression> guards ) {
		pexp.left.transition(guards)+pexp.right.transition(guards)
	}
	
	def dispatch Iterable<Transition> transition( ProcessExpressionGuard pexp , Iterable<Expression> guards ) {		
		var newguards = guards.clone
		newguards.add( pexp.guard.booleanExpression )		
		pexp.expression.transition( newguards )
	}
	
	def dispatch Iterable<Transition> transition( ProcessExpressionAction pexp , Iterable<Expression> guards ) {		
		var next = pexp.next 		
		switch next {
			ProcessExpressionNil: newLinkedList( new Transition( guards , pexp.action ) )
			ProcessExpressionKill: newLinkedList( new Transition( guards , pexp.action , true ) ) 
			ProcessExpressionReference: newLinkedList( 
				new Transition(
					guards ,
					pexp.action , 
					new ProcessDerivative( next.expression )	
				)				
			)
		}
		
	}
	
}