package eu.quanticol.carma.ui.views;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.simulator.CarmaSystem;

public class ResultsProvider {
	
	private ArrayList<Result> results;
	private SimulationTimeSeries sts;
	
	public ResultsProvider(StatisticSampling<CarmaSystem> ss, int sampling){
		
		results = new ArrayList<Result>();
		
		sts = ss.getSimulationTimeSeries().get(0);
		
		for(int i = 0; i < sampling; i++){
			results.add(new Result(sts.getName(),sts.getTime(i),sts.getMean(sts.getTime(i)),sts.getStandardDeviation(i)));
		}
		
	}
	
	public ArrayList<Result> getResults(){
		return results;
	}
	
	public String toCSVString(){
		
		String toReturn = sts.getName()+";Time"+";Mean"+"Standard Deviation";
		
		for(Result result : results){
			toReturn = toReturn +"\n;"+result.toCSVString();
		}
		
		return toReturn;
	}

}
