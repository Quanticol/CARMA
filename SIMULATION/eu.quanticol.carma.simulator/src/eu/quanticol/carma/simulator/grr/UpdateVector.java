/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

import java.util.LinkedList;

/**
 * @author loreti
 *
 */
public class UpdateVector {
	
	private LinkedList<UpdateElement> updates;

	public UpdateVector() {
		this.updates = new LinkedList<>();
	}
	
	public void addUpdate( Agent agent , Configuration configuration , int update ) {
		updates.add( new UpdateElement(new Instance(agent, configuration), update) );
	}
	
	public void apply( Population population ) {
		for (UpdateElement updateElement : updates) {
			population.update( updateElement.instance , updateElement.update );
		}
	}
	
	private class UpdateElement {
		
		public final Instance instance;
		public final int update;
		
		public UpdateElement( Instance instance , int update ) {
			this.instance = instance;
			this.update = update;
		}
		
	}
	
}

