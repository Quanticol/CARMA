package eu.quanticol.carma.tools.fluid

import java.util.Set
import java.util.Map
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.InputAction

class DerivativeSet {
	
	Set<ProcessDerivative> derivatives
	
	Map<ProcessDerivative,Iterable<Transition>> transitions
	
	new( ProcessState state ) {
		populate( state )
	}
	
	def populate( ProcessState state ) {
		var first = new ProcessDerivative(state) 
		derivatives = newHashSet( first )
		transitions = newHashMap()
		var pending = newLinkedList( first )
		while (!pending.empty) {
			var aState = pending.poll
			if (!derivatives.contains(aState)) {
				derivatives.add(aState)
				var next = aState.transitions
				transitions.put( aState , next )
				pending.addAll(next.map[ it.next ].filter[ !derivatives.contains(it) ])
			}
		}
	}
	
	def outputActivities( boolean broadcast ) {
		transitions.values.map[ it.map[ it.action ] ]
			.flatten
			.filter( typeof(OutputAction) )
			.filter[ it.activity.isBroadacst == broadcast ]
			.map[ it.activity.name ]
			.toSet
	}
	
	def broadcastOutputActivities( ) {
		outputActivities(true)
	}
	
	def unicastOutputActivities( ) {
		outputActivities(false)
	}
	
	def inputActivities( boolean broadcast ) {
		transitions.values.map[ it.map[ it.action ] ]
			.flatten
			.filter( typeof(InputAction) )
			.filter[ it.activity.isBroadacst == broadcast ]
			.map[ it.activity.name ]
			.toSet
	}
	
	def broadcastInputActivities( ) {
		outputActivities(true)
	}
	
	def unicastInputActivities( ) {
		outputActivities(false)
	}
		
}