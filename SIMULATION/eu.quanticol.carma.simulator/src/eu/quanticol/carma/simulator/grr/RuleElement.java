/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

import java.util.function.Function;

/**
 * @author loreti
 *
 */
public class RuleElement {

	private final boolean isForAll;
	private final Agent source;
	private final Agent target;
	private final Function<Configuration, Boolean> guard;
	private final Function<Configuration,Configuration> next;
	private final WeightFunction weight;
	
	public RuleElement( WeightFunction weight , Agent source , Function<Configuration,Boolean> guard , Agent target , Function<Configuration, Configuration> next ) {
		this( false , weight , source , guard , target , next );
	}

	public RuleElement( boolean isForAll , WeightFunction weight, Agent source , Function<Configuration,Boolean> guard , Agent target , Function<Configuration, Configuration> next ) {
		this.isForAll = isForAll;
		this.source = source;
		this.target = target;
		this.guard = guard;
		this.next = next;
		this.weight = weight;
	}

	public boolean isForAll() {
		return isForAll;
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
