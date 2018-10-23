/**
 * 
 */
package org.cmg.ml.sam.sim.tests.pm;

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.function.Function;

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
public class GossipFOSSACS_B {
	
	private static final int SIM_TIME = 20;
	
	private static final int SAMPLINGS = 100;

	private static final double DIFF_RATE = 1.0;
	private static final double PASS_RATE = 0.05;
	private static final double PROB = 0.01;
	private static final double LOST_PROB = 0.75;
	private static final double K = 5.0;
	private static final double DEADLINE = 10.0;
	private static final int ITERATIONS = 1;

	private static final int PU_INIT_SIZE = 81;
	private static final int PI_INIT_SIZE = 9;
	private static final int AU_INIT_SIZE = 9;
	private static final int AI_INIT_SIZE = 1;

	
	public static void main(String[] argv) throws FileNotFoundException {
//		run("const_",ITERATIONS,DEADLINE,SAMPLINGS,s -> DIFF_RATE,s -> PASS_RATE, s-> PROB,getInitState(1));
//
		run(
				"bc_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);
		run(
				"bc_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);
		run(
				"bc_10_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(10)
			);

		run(
			"bc_100_",
			ITERATIONS,
			DEADLINE,SAMPLINGS,
			getInitState(100)
		);
	
		run(
				"bc_1000_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1000)
			);
//		
//		run(
//				"bc_10000_",
//				ITERATIONS,
//				DEADLINE,SAMPLINGS,
//				getInitState(10000)
//			);

}
	
	private static int[] getInitState(int scale) {
		return new int[] { PI_INIT_SIZE*scale, PU_INIT_SIZE*scale, AI_INIT_SIZE*scale, AU_INIT_SIZE*scale };
	}

	public static void run( 
			String label , 
			int iterations, 
			double deadline,
			int samplings,
			int[] initial
	) throws FileNotFoundException {
		SimulationEnvironment<PopulationModel> sim = new SimulationEnvironment<PopulationModel>(
				new GossipFactoryFOSSACS_B(initial,10,0.01,1.0,0.05)
			);
			StatisticSampling<PopulationModel> puSamp = new StatisticSampling<>(samplings, deadline/samplings, 
				new Measure<PopulationModel>() {

					@Override
					public double measure(PopulationModel t) {
						// TODO Auto-generated method stub
						return t.getCurrentState().getOccupancy(GossipFactoryFOSSACS_B.PU_INDEX);
					}
		
					@Override
					public String getName() {
						return "PU";
					}

				});
			StatisticSampling<PopulationModel> piSamp = new StatisticSampling<>(samplings, deadline/samplings, 
					new Measure<PopulationModel>() {

						@Override
						public double measure(PopulationModel t) {
							// TODO Auto-generated method stub
							return t.getCurrentState().getOccupancy(GossipFactoryFOSSACS_B.PI_INDEX);
						}
			
						@Override
						public String getName() {
							return "PI";
						}

					});
			StatisticSampling<PopulationModel> auSamp = new StatisticSampling<>(samplings, deadline/samplings, 
					new Measure<PopulationModel>() {

						@Override
						public double measure(PopulationModel t) {
							// TODO Auto-generated method stub
							return t.getCurrentState().getOccupancy(GossipFactoryFOSSACS_B.AU_INDEX);
						}
			
						@Override
						public String getName() {
							return "AU";
						}

					});
			StatisticSampling<PopulationModel> aiSamp = new StatisticSampling<>(samplings, deadline/samplings, 
					new Measure<PopulationModel>() {

						@Override
						public double measure(PopulationModel t) {
							// TODO Auto-generated method stub
							return t.getCurrentState().getOccupancy(GossipFactoryFOSSACS_B.AI_INDEX);
						}
			
						@Override
						public String getName() {
							return "AI";
						}

					});



			long start = System.currentTimeMillis();
			sim.setSampling(new SamplingCollection<>(puSamp, piSamp, auSamp, aiSamp));
			sim.simulate(iterations,DEADLINE);
			System.out.println("Time: "+(System.currentTimeMillis()-start));
			puSamp.printTimeSeries(new PrintStream("data/"+label+"_pu_.data"));
			piSamp.printTimeSeries(new PrintStream("data/"+label+"_pi_.data"));
			auSamp.printTimeSeries(new PrintStream("data/"+label+"_au_.data"));
			aiSamp.printTimeSeries(new PrintStream("data/"+label+"_ai_.data"));
	}
}
