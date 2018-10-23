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
public class Gossip {
	
	private static final int SIM_TIME = 20;
	
	private static final int SAMPLINGS = 100;

	private static final double DIFF_RATE = 1.0;
	private static final double PASS_RATE = 0.05;
	private static final double PROB = 0.25;
	private static final double LOST_PROB = 0.75;
	private static final double K = 5.0;
	private static final double DEADLINE = 30.0;
	private static final int ITERATIONS = 100;

	private static final int PU_INIT_SIZE = 60;
	private static final int PI_INIT_SIZE = 20;
	private static final int AU_INIT_SIZE = 15;
	private static final int AI_INIT_SIZE = 5;

	
	public static void main(String[] argv) throws FileNotFoundException {
//		run("const_",ITERATIONS,DEADLINE,SAMPLINGS,s -> DIFF_RATE,s -> PASS_RATE, s-> PROB,getInitState(1));
//
		run(
				"bc_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				s -> DIFF_RATE,
				s -> PASS_RATE, 
				s-> (s.getOccupancy(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX)>0?K/(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX):1.0),
				getInitState(1)
			);

		run(
			"bc_5_",
			ITERATIONS,
			DEADLINE,SAMPLINGS,
			s -> DIFF_RATE,
			s -> PASS_RATE, 
			s-> (s.getOccupancy(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX)>0?K/(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX):1.0),
			getInitState(5)
		);
	
		run(
				"bc_10_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				s -> DIFF_RATE,
				s -> PASS_RATE, 
				s-> (s.getOccupancy(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX)>0?K/(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX):1.0),
				getInitState(10)
			);

		run(
				"bc_20_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				s -> DIFF_RATE,
				s -> PASS_RATE, 
				s-> (s.getOccupancy(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX)>0?K/(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX):1.0),
				getInitState(20)
			);

		run(
				"bc_50_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				s -> DIFF_RATE,
				s -> PASS_RATE, 
				s-> (s.getOccupancy(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX)>0?K/(GossipFactory.PU_INDEX)+s.getOccupancy(GossipFactory.PI_INDEX):1.0),
				getInitState(50)
			);
}
	
	private static int[] getInitState(int scale) {
		return new int[] { PI_INIT_SIZE*scale, PU_INIT_SIZE*scale, AI_INIT_SIZE*scale, AU_INIT_SIZE*scale };
	}

	public static void run( 
			String label , 
			int iterations, 
			double deadline,
			int samplings,
			Function<PopulationState,Double> diffFunction,
			Function<PopulationState,Double> passiveFunction,
			Function<PopulationState,Double> recFunction,
			int[] initial
	) throws FileNotFoundException {
		SimulationEnvironment<PopulationModel> sim = new SimulationEnvironment<PopulationModel>(
				new GossipFactory(initial,diffFunction,passiveFunction,recFunction)
			);
			StatisticSampling<PopulationModel> puSamp = new StatisticSampling<>(samplings, deadline/samplings, 
				new Measure<PopulationModel>() {

					@Override
					public double measure(PopulationModel t) {
						// TODO Auto-generated method stub
						return t.getCurrentState().getOccupancy(GossipFactory.PU_INDEX);
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
							return t.getCurrentState().getOccupancy(GossipFactory.PI_INDEX);
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
							return t.getCurrentState().getOccupancy(GossipFactory.AU_INDEX);
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
							return t.getCurrentState().getOccupancy(GossipFactory.AI_INDEX);
						}
			
						@Override
						public String getName() {
							return "AI";
						}

					});



			sim.setSampling(new SamplingCollection<>(puSamp, piSamp, auSamp, aiSamp));
			sim.simulate(iterations,DEADLINE);
			puSamp.printTimeSeries(new PrintStream("data/"+label+"_pu_.data"));
			piSamp.printTimeSeries(new PrintStream("data/"+label+"_pi_.data"));
			auSamp.printTimeSeries(new PrintStream("data/"+label+"_au_.data"));
			aiSamp.printTimeSeries(new PrintStream("data/"+label+"_ai_.data"));
	}
}
