/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

import java.util.function.Function;

/**
 * @author loreti
 *
 */
public class UnicastInteraction {

	private final Agent source;
	private final Function<Configuration, Boolean> guard;
	private final Agent target;
	private final Function<Configuration,Configuration> next;
	private final WeightFunction weight;
	
	public UnicastInteraction( WeightFunction weight, Agent source , Function<Configuration,Boolean> guard , Agent target , Function<Configuration, Configuration> next ) {
		this.source = source;
		this.target = target;
		this.guard = guard;
		this.next = next;
		this.weight = weight;
	}

	public Agent getSource() {
		return source;
	}

	public Agent getTarget() {
		return target;
	}

	public Function<Configuration, Boolean> getGuard() {
		return guard;
	}

	public Function<Configuration, Configuration> getNext() {
		return next;
	}

	
}
