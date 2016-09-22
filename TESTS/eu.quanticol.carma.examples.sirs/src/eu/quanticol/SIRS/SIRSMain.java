package eu.quanticol.SIRS;

import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.SIRS.SIRS.*;
import eu.quanticol.SIRS.SIRSFactory;
import eu.quanticol.carma.simulator.CarmaSystem;

public class SIRSMain {
	
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new SIRSFactory(1000,1,0,4)
		);
		StatisticSampling<CarmaSystem> sus = new StatisticSampling<CarmaSystem>(101, 1.0, new SusceptiblesMeasure(0) );
		StatisticSampling<CarmaSystem> inf = new StatisticSampling<CarmaSystem>(101, 1.0, new InfectivesMeasure(0) );
		StatisticSampling<CarmaSystem> rec = new StatisticSampling<CarmaSystem>(101, 1.0, new RecoveredsMeasure(0) );
		system.setSampling( new SamplingCollection<CarmaSystem>( sus, inf, rec ) );
		system.simulate(200,100);
		sus.printTimeSeries(System.out);
		inf.printTimeSeries(System.out);
		rec.printTimeSeries(System.out);
	}

}
