package eu.quanticol.carma.ui.views;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.simulator.CarmaSystem;

public class ResultsProvider {
	
	private ArrayList<Result> results;
	
	public ResultsProvider(StatisticSampling<CarmaSystem> ss, int sampling){
		
		results = new ArrayList<Result>();
		
		SimulationTimeSeries sts = ss.getSimulationTimeSeries().get(0);
		
		for(int i = 0; i < sampling; i++){
			results.add(new Result(sts.getName(),sts.getTime(i),sts.getMean(sts.getTime(i)),sts.getStandardDeviation(i)));
		}
		
	}
	
	public ArrayList<Result> getResults(){
		return results;
	}

}
