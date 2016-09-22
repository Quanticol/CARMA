package eu.quanticol.carma.tools.fluid

import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.Action
import java.util.Map

class Transition {
	
	Iterable<Expression> guards
	
	Action action
	
	ProcessDerivative next
	
	boolean isKill

	new( Iterable<Expression> guards , Action action , ProcessDerivative next , boolean isKill ) {
		this.guards = guards
		this.action = action
		this.next = next
		this.isKill = isKill
	}
	
	new( Iterable<Expression> guards , Action action , ProcessDerivative next ) {
		this( guards, action, next, false )
	}

	new( Iterable<Expression> guards , Action action ) {
		this( guards, action, null, false )
	}

	new( Iterable<Expression> guards , Action action , boolean isKill ) {
		this( guards, action, null, true )
	}
	
	def getGuards( ) {
		guards
	}
	
	def getNext( ) {
		next
	}
	
	def getAction( ) {
		action	
	}
	
	def isKill( ) {
		(next == null)&&isKill	
	}
	
	def isNill( ) {
		(next == null)&&(!isKill)
	}
	
	def combine( ProcessDerivative p ) {
		if (this.isKill) {
			this	
		} else if (this.isNill) {
			new Transition( guards , action , p , false )
		} else if (p == null) {	
			this
		} else {
			new Transition( guards , action , new ProcessDerivative( next , p ) )		
		}
	}
	
	def isEnabled( Map<String,Object> store ) {
		guards.forall[]		
	}
}