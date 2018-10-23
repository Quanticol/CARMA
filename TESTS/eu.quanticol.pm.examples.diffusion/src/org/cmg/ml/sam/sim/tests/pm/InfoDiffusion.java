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
public class InfoDiffusion {
	
	private static final int SIM_TIME = 20;
	
	private static final int SAMPLINGS = 1000;

	private static final double RATE = 0.1;
	private static final double PROB = 0.2;
	private static final double K = 5.0;
	private static final double DEADLINE = 50.0;
	private static final int ITERATIONS = 100;

	private static final int U_INIT_SIZE = 99;

	private static final int B_INIT_SIZE = 1;

	private static final int I_INIT_SIZE = 0;
	
	public static void main(String[] argv) throws FileNotFoundException {
		run("const_sc_1",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(1));
		run("const_sc_5",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(5));
		run("const_sc_10",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(10));
		run("const_sc_20",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(20));
		run("const_sc_50",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(50));
		run("const_sc_100",ITERATIONS,DEADLINE,SAMPLINGS,s -> RATE,s-> PROB,getInitState(50));
		
		run("pb_sc_1",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(1)
		);
				
		run("pb_sc_5",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(5));
		run("pb_sc_10",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(10));
		run("pb_sc_20",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(20));
		run("pb_sc_50",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(50));
		run("pb_sc_100",ITERATIONS,DEADLINE,SAMPLINGS,
				s -> RATE,
				s-> (s.getOccupancy(InfoDiffusionFactory.U_INDEX)<K?1.0:K/s.getOccupancy(InfoDiffusionFactory.U_INDEX)),
				getInitState(100));
		
	}
	
	private static int[] getInitState(int scale) {
		return new int[] {U_INIT_SIZE*scale,B_INIT_SIZE*scale,I_INIT_SIZE*scale};
	}

	public static void run( 
			String label , 
			int iterations, 
			double deadline,
			int samplings,
			Function<PopulationState,Double> rateFunction,
			Function<PopulationState,Double> recFunction,
			int[] initial
	) throws FileNotFoundException {
		SimulationEnvironment<PopulationModel> sim = new SimulationEnvironment<PopulationModel>(
				new InfoDiffusionFactory(initial,rateFunction,recFunction)
			);
			StatisticSampling<PopulationModel> uSamp = new StatisticSampling<>(samplings, deadline/samplings, 
				new Measure<PopulationModel>() {

					@Override
					public double measure(PopulationModel t) {
						// TODO Auto-generated method stub
						return t.getCurrentState().getOccupancy(InfoDiffusionFactory.U_INDEX);
					}
		
					@Override
					public String getName() {
						return "U";
					}

				});
			StatisticSampling<PopulationModel> iSamp = new StatisticSampling<>(samplings, deadline/samplings, 
					new Measure<PopulationModel>() {

						@Override
						public double measure(PopulationModel t) {
							// TODO Auto-generated method stub
							return t.getCurrentState().getOccupancy(InfoDiffusionFactory.I_INDEX);
						}
			
						@Override
						public String getName() {
							return "I";
						}

					});
			StatisticSampling<PopulationModel> bSamp = new StatisticSampling<>(samplings, deadline/samplings, 
					new Measure<PopulationModel>() {

						@Override
						public double measure(PopulationModel t) {
							// TODO Auto-generated method stub
							return t.getCurrentState().getOccupancy(InfoDiffusionFactory.B_INDEX);
						}
			
						@Override
						public String getName() {
							return "B";
						}

					});


			sim.setSampling(new SamplingCollection<>(uSamp, iSamp, bSamp));
			sim.simulate(iterations,50);
			uSamp.printTimeSeries(new PrintStream("data/"+label+"_u_.data"));
			iSamp.printTimeSeries(new PrintStream("data/"+label+"_i_.data"));
			bSamp.printTimeSeries(new PrintStream("data/"+label+"_b_.data"));
	}
}
