/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.function.Function;

/**
 * @author loreti
 *
 */
public class Population {
	
	private HashMap<Agent, HashMap<Configuration, Integer> > population;
	
	
	public Population() {
		this.population = new HashMap<>();
	}
	
	public Population( Instance ... instances ) {
		this();
		for( int i = 0; i<instances.length ; i++ ) {
			add( instances[i] );
		}
	}

	public void add(Instance instance) {
		update( instance , 1 );
	}
	
	public void update( Instance instance , int update ) {
		if (update == 0) {
			return ;
		}
		HashMap<Configuration,Integer> nestedMap = this.population.get( instance.getSpecie() );
		if (nestedMap == null) {
			nestedMap = new HashMap<>();
			this.population.put(instance.getSpecie(), nestedMap);
		} 
		Integer value = nestedMap.get(instance.getConfiguration());
		if (value == null) {
			value = 0;
		} 
		value = value + update;
		if (value > 0) {
			nestedMap.put(instance.getConfiguration(), value);
		} 
		if (value <= 0) {
			nestedMap.remove(instance.getConfiguration());
		}
	}

	public void update( Variation v ) {
		this.update(v.getInstance(),v.getVariation());
	}
	
	public int elements( Agent species , Function<Configuration, Boolean> filter ) {
		HashMap<Configuration,Integer> innerMap = population.get(species);
		if (innerMap == null) {
			return 0;
		}
		int toReturn = 0;
		for (Entry<Configuration, Integer> entry : innerMap.entrySet()) {
			if (filter.apply(entry.getKey())) {
				toReturn += entry.getValue();
			}
		}
		return toReturn;
	}

	public void update(Agent agent, Configuration configuration, int update) {
		
	}
}
