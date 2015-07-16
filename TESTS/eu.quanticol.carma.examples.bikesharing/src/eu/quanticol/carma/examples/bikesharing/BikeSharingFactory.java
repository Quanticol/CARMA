/**
 * 
 */
package eu.quanticol.carma.examples.bikesharing;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.Agent;
import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class BikeSharingFactory implements SimulationFactory<CarmaSystem> {
	
	private int bikers = 0;
	private int pedestrians = 0;
	private int parking_stations = 0;
	private int slots = 0;
	private int bikes = 0;
	
	public BikeSharingFactory(int bikers , int pedestrians, int parking_stations , int slots , int bikes ) {
		this.bikers = bikers;
		this.pedestrians = pedestrians;
		this.parking_stations = parking_stations;
		this.slots = slots;
		this.bikes = bikes;
	}

	@Override
	public CarmaSystem getModel() {
		return new BikeSharing( bikers , pedestrians , parking_stations , bikes , slots );
	}

	@Override
	public Measure<CarmaSystem> getMeasure(String name) {
		// TODO Auto-generated method stub
		//FIXME!!!!
		return null;
	}
	
//	@Override
//	public BikeSharing getSimulationContext() {
//		return new BikeSharing( bikers , pedestrians , parking_stations , bikes , slots );
//	}
//
//	@Override
//	public ArrayList<Agent<CaspaSystem>> getAgents(CaspaSystem context) {
//		return context.getAgents();
//	}

}
