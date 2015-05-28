/**
 * 
 */
package eu.quanticol.carma.examples.gc;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.SamplingLog;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class GroupiesAndCelebrities extends CarmaSystem {
	
	@Override
	public String toString() {
		int kindA = this.count(new CarmaPredicate() {
			
			@Override
			public boolean satisfy(CarmaStore store) {
				Integer aType = GroupiesCelebritiesDefinitions.KIND_A;
				return aType.equals(store.get(GroupiesCelebritiesDefinitions.KIND_ATTRIBUTE, Integer.class));
			}
		});
		int kindB = this.count(new CarmaPredicate() {
			
			@Override
			public boolean satisfy(CarmaStore store) {
				Integer aType = GroupiesCelebritiesDefinitions.KIND_B;
				return aType.equals(store.get(GroupiesCelebritiesDefinitions.KIND_ATTRIBUTE, Integer.class));
			}
		});
		
		return "<"+kindA+","+kindB+">";
	}

	public static final int KIND_A = 0;
	public static final int KIND_B = 1;
	
	public static final int ACT_ID = 0;
	public static final double BROADCAST_LOST_PROBABILITY = 0.75;
	public static final double UNICAST_LOST_PROBABILITY = 0;
	public static final double BROADCAST_RATE = 0.1;
	public static final double UNICAST_RATE = 1.0;

	public GroupiesAndCelebrities(int groupies, int celebrities) {
		for( int i=0 ; i<groupies ; i++ ) {
			CarmaComponent c = new CarmaComponent();
			c.addAgent( GroupiesCelebritiesDefinitions.getGroupyProcess(c) );
			c.set(GroupiesCelebritiesDefinitions.KIND_ATTRIBUTE, i%2);
			addComponent( c );
		}
		for( int i=0 ; i<celebrities ; i++ ) {
			CarmaComponent c = new CarmaComponent();
			c.addAgent( GroupiesCelebritiesDefinitions.getCelebrityProcess(c) );
			c.set(GroupiesCelebritiesDefinitions.KIND_ATTRIBUTE, i%2);
			addComponent( c );
		}
	}

	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return 1-BROADCAST_LOST_PROBABILITY;
	}

	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return 1-UNICAST_LOST_PROBABILITY;
	}

	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		return BROADCAST_RATE;
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		return UNICAST_RATE;
	}

	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action, Object value) {
	}

	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, CarmaStore receiver, int action, Object value) {
	}

	public static void main(String[] main) {
		SimulationEnvironment<CarmaSystem> simEnv = new SimulationEnvironment<CarmaSystem>(new GroupiesAndCelebritiesFactory(100,0));
		simEnv.setSampling(new SamplingLog<CarmaSystem>(0.1));
		simEnv.simulate(100.0);
	}

}
