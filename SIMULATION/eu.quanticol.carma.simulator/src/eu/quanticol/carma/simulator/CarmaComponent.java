/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.LinkedList;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;

import eu.quanticol.carma.simulator.space.Node;

/**
 * @author loreti
 *
 */
public class CarmaComponent {
	
	protected CarmaStore store;

	protected LinkedList<CarmaProcess> processes;
	
	protected boolean isKilled = false;
	
	public CarmaComponent( ) {
		this.store = new CarmaStore();
		this.processes = new LinkedList<CarmaProcess>();
	}

	public void set(String attribute, Object value) {
		store.set(attribute, value);
	}

	public <T> T get(String attribute, Class<T> clazz) {
		return store.get(attribute, clazz);
	}

	public void setLocation( Node l ) {
		this.set( CarmaSystem.LOC_ATTRIBUTE_NAME , l);
	}
	
	public Node getLocation() {
		return get( CarmaSystem.LOC_ATTRIBUTE_NAME , Node.class );
	}
	
	public void inputBroadcast( RandomGenerator r , 
			CarmaSystem system,
			CarmaComponent sender, 
			int action, 
			CarmaPredicate predicate,
			Object value ) {
		if (predicate.satisfy(system.now(),store)) {
			WeightedStructure<Activity> enabledInputs = new ComposedWeightedStructure<Activity>();
			double missingProbability = 1.0;
			for (CarmaProcess p : processes) {//TODO: fix this!
				WeightedStructure<Activity> foo = p.doReceiveBroadcast(system, sender.store , action, value );
				missingProbability = missingProbability*(1-foo.getTotalWeight());
				enabledInputs = enabledInputs.add(foo);
			}
			double select = r.nextDouble()*(enabledInputs.getTotalWeight()+missingProbability);
			if (select<enabledInputs.getTotalWeight()) {
				enabledInputs.select(select).getElement().execute(r);
			} 
		}
	}
	
	public WeightedStructure<Activity> inputUnicast( CarmaSystem system ,
			CarmaComponent sender ,
			int action ,
			CarmaPredicate predicate ,
			Object value ) {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		if (predicate.satisfy(system.now(),store)) {
			for (CarmaProcess p : processes) {
				toReturn = toReturn.add( p.doReceiveUnicast(system, sender.store, action, value) );
			}
		}
		return toReturn;
	}

	public void addAgent(CarmaProcess process) {
		process.setComponent(this);
		processes.add(process);
	}

	public CarmaStore getStore() {
		return store;
	}

	public WeightedStructure<Activity> getActivities(CarmaSystem caspaSystem) {
		WeightedStructure<Activity> toReturn = new ComposedWeightedStructure<Activity>();
		for (CarmaProcess caspaProcess : processes) {
			toReturn = toReturn.add( caspaProcess.getActivities( caspaSystem ) );
		}
		return toReturn;
	}

	protected void setStore(CarmaStore store) {
		this.store = store;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return store.toString()+":"+processes.toString();
	}

	public boolean isRunning(CarmaProcessPredicate caspaProcessPredicate) {
		for (CarmaProcess caspaProcess : processes) {
			if (caspaProcessPredicate.eval(caspaProcess)) {
				return true;
			}
		}
		return false;
	}

	public boolean kill() {
		this.isKilled = true;
		return this.isKilled;
	}

	public boolean isKilled() {
		return isKilled;
	}


}
