/**
 * 
 */
package org.cmg.ml.sam.sim.tests.pm;

import java.util.LinkedList;

import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.SamplingFunction;
import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

/**
 * @author loreti
 *
 */
public class PMSier {
	
	private static final int SIZE = 100;

	public static void main(String[] argv) {
		SimulationEnvironment<PopulationModel> sim = new SimulationEnvironment<PopulationModel>(
			new SierFactory(SIZE)
		);
		StatisticSampling<PopulationModel> sSamp = new StatisticSampling<>(1000, 0.1, 
			new Measure<PopulationModel>() {

				@Override
				public double measure(PopulationModel t) {
					// TODO Auto-generated method stub
					return t.getCurrentState().getOccupancy(SierFactory.S_INDEX);
				}
	
				@Override
				public String getName() {
					return "S";
				}

			});
		StatisticSampling<PopulationModel> iSamp = new StatisticSampling<>(1000, 0.1, 
				new Measure<PopulationModel>() {

					@Override
					public double measure(PopulationModel t) {
						// TODO Auto-generated method stub
						return t.getCurrentState().getOccupancy(SierFactory.I_INDEX);
					}
		
					@Override
					public String getName() {
						return "I";
					}

				});
		StatisticSampling<PopulationModel> rSamp = new StatisticSampling<>(1000, 0.1, 
				new Measure<PopulationModel>() {

					@Override
					public double measure(PopulationModel t) {
						// TODO Auto-generated method stub
						return t.getCurrentState().getOccupancy(SierFactory.R_INDEX);
					}
		
					@Override
					public String getName() {
						return "R";
					}

				});


		sim.setSampling(new SamplingCollection<>(sSamp, iSamp, rSamp));
		System.out.println(sim.simulate(100));
	}
	
}
