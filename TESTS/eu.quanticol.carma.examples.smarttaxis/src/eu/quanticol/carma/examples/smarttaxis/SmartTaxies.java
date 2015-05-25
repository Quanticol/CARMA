package eu.quanticol.carma.examples.smarttaxis;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.examples.smarttaxis.SmartTaxiesDefinitions.InfoClass;
import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaProcessPredicate;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.simulator.ComponentPredicate;

public class SmartTaxies extends CarmaSystem {
	
	
	public SmartTaxies( int taxies , int users ) {
		
		for( int l=0 ; l<SmartTaxiesDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
			for( int i=0 ; i<taxies ; i++ ) {
				CarmaComponent taxi = getTaxi( l );
				addComponent(taxi);
			}
		}
		
		for( int l=0 ; l<SmartTaxiesDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
			CarmaComponent arrival = getArrivalComponent( l );
			addComponent( arrival );
		}
		
//		for( int l=0 ; l<SmartTaxiesDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
//			for( int i=0 ; i<users ; i++ ) {
//				CaspaComponent user = getUserComponent( l );
//				addComponent( user );
//			}
//		}
		
	}

	private CarmaComponent getArrivalComponent(int l) {
		CarmaComponent c = new CarmaComponent();
		c.set( SmartTaxiesDefinitions.LOC_ATTRIBUTE ,  l );
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxiesDefinitions.ArrivalProcess ) );
		return c;
	}

	private CarmaComponent getTaxi(int loc) {
		CarmaComponent c = new CarmaComponent();
		c.set( SmartTaxiesDefinitions.LOC_ATTRIBUTE ,  loc );
		c.set( SmartTaxiesDefinitions.INFO_ATTRIBUTE ,  new SmartTaxiesDefinitions.InfoClass( SmartTaxiesDefinitions.NUMBER_OF_LOCATIONS ) );
		c.set( SmartTaxiesDefinitions.DEST_ATTRIBUTE ,  -1 );
		c.set( SmartTaxiesDefinitions.OCCUPIED_ATTRIBUTE, false);
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxiesDefinitions.TaxiProcess ) );
//		c.addAgent( new CaspaSequentialProcess(c, SmartTaxiesDefinitions.InfoProcess ) );
		return c;
	}

	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return SmartTaxiesDefinitions.P_LOST;
	}

	private double getMoveTime(double now, Integer from, Integer to) {
		return SmartTaxiesDefinitions.STEP_RATE*SmartTaxiesDefinitions.steps(from,to);
	}

	@Override
	public double unicastProbability(final CarmaStore sender, CarmaStore receiver,
			int action) {
		if (action == SmartTaxiesDefinitions.TAKE) {
			double foo = measure(new ComponentPredicate() {

				@Override
				public boolean eval(CarmaComponent c) {
					int loc = sender.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE);
					if (c.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE).equals(loc)) {
						return c.isRunning( new CarmaProcessPredicate() {

							@Override
							public boolean eval(CarmaProcess p) {
								if (p instanceof CarmaSequentialProcess) {
									CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
									return csp.getName().equals("Taxi")&&csp.getState().getName().equals("F");
								}
								return false;
							}
							
						});
					}
					return false;
				}
				
			});
			if (foo != 0) {
				return 1/foo;
			}
		}
		if (action == SmartTaxiesDefinitions.CALL) {
			return 1.0;
		}
		return 0;
	}

	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		if (action == SmartTaxiesDefinitions.CALL) {
			return SmartTaxiesDefinitions.CALL_RATE;
		}
		if (action == SmartTaxiesDefinitions.EXCH) {
			SmartTaxiesDefinitions.InfoClass info = sender.get(SmartTaxiesDefinitions.INFO_ATTRIBUTE, SmartTaxiesDefinitions.INFO_ATTRIBUTE_TYPE);
			if ((info == null)||(info.elements()==0)) {
				return 0.0;
			} else {
				return SmartTaxiesDefinitions.EXCH_RATE;
			}
		}
		if (action == SmartTaxiesDefinitions.CHANGE) {
			InfoClass ic = sender.get(SmartTaxiesDefinitions.INFO_ATTRIBUTE, SmartTaxiesDefinitions.INFO_ATTRIBUTE_TYPE);
			int loc = sender.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE);
			return SmartTaxiesDefinitions.CHANGE_RATE*ic.getMovingProbability( loc );
		}
		if (action == SmartTaxiesDefinitions.MOVE) {
			return getMoveTime( 
				now() , 
				sender.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE ) ,
				sender.get(SmartTaxiesDefinitions.DEST_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE ) 
			);
		}
		if (action == SmartTaxiesDefinitions.AGE) {
			SmartTaxiesDefinitions.InfoClass info = sender.get(SmartTaxiesDefinitions.INFO_ATTRIBUTE, SmartTaxiesDefinitions.INFO_ATTRIBUTE_TYPE);
			if ((info == null)||(info.elements()==0)) {
				return 0.0;
			} else {
				return SmartTaxiesDefinitions.AGE_RATE;
			}
		}
		if (action == SmartTaxiesDefinitions.ARRIVE) {
			return SmartTaxiesDefinitions.arrivalRate( now() , sender.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE ) );
		}
		return 0;
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (action == SmartTaxiesDefinitions.TAKE) {
			return SmartTaxiesDefinitions.TAKE_RATE;
		}
		if (action == SmartTaxiesDefinitions.CALL) {
			return SmartTaxiesDefinitions.CALL_RATE;
		}
		return 0;
	}

	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action) {
		if (action == SmartTaxiesDefinitions.ARRIVE) {
			int loc = sender.get(SmartTaxiesDefinitions.LOC_ATTRIBUTE, SmartTaxiesDefinitions.LOC_ATTRIBUTE_TYPE);
			int dest = SmartTaxiesDefinitions.getDestination( now , loc , r );
			addComponent( 
					getUserComponent( 
							loc ,
							dest//r.nextInt(SmartTaxiesDefinitions.NUMBER_OF_LOCATIONS)
					));
		}
	}

	private CarmaComponent getUserComponent(int loc , int dest ) {
		CarmaComponent c = new CarmaComponent();
		c.set(SmartTaxiesDefinitions.LOC_ATTRIBUTE, loc);
		c.set(SmartTaxiesDefinitions.DEST_ATTRIBUTE, dest);
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxiesDefinitions.UserProcess) );
		return c;
	}

	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, int action) {

	}

	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new SmartTaxiesFactory(SmartTaxiesDefinitions.TAXIES, 0)
		);
		int deadline = (int) SmartTaxiesDefinitions.LIMIT;
//		StatisticSampling<CaspaSystem> taxiesAtLoc = new StatisticSampling<CaspaSystem>(101, 1.0, new MinAvailableBikes(0) );
//		StatisticSampling<CaspaSystem> maxBikes = new StatisticSampling<CaspaSystem>(101, 1.0, new MaxAvailableBikes(0) );
//		StatisticSampling<CaspaSystem> averageBikes = new StatisticSampling<CaspaSystem>(101, 1.0, new AverageAvailableBikes(0) );
//		system.setSampling( new SamplingCollection<CaspaSystem>( minBikes , maxBikes , averageBikes ) );
		StatisticSampling<CarmaSystem> waitingUsers0 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxiesDefinitions.getMeasureOfWaitingUsers(SmartTaxiesDefinitions.getLocId(1, 1)));
		StatisticSampling<CarmaSystem> freeTaxies0 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxiesDefinitions.getMeasureOfAvailabelTaxies(SmartTaxiesDefinitions.getLocId(1, 1)));
		StatisticSampling<CarmaSystem> movingTaxies = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxiesDefinitions.getMeasureOfMovingTaxies());
		StatisticSampling<CarmaSystem> waitingUsers1 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxiesDefinitions.getMeasureOfWaitingUsers(SmartTaxiesDefinitions.getLocId(0, 0)));
		StatisticSampling<CarmaSystem> freeTaxies1 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxiesDefinitions.getMeasureOfAvailabelTaxies(SmartTaxiesDefinitions.getLocId(0, 0)));
		system.setSampling( new SamplingCollection<CarmaSystem>( waitingUsers0 , waitingUsers1 ,movingTaxies, freeTaxies0 , freeTaxies1 ) );
		system.simulate(200,deadline);
		System.out.println("WAITING AT (1,1):");
		waitingUsers0.printTimeSeries(System.out);
		System.out.println("\n\nWAITING AT (0,0):");
		waitingUsers1.printTimeSeries(System.out);
		System.out.println("FREE AT (1,1):");
		freeTaxies0.printTimeSeries(System.out);
		System.out.println("FREE AT (1,1):");
		freeTaxies1.printTimeSeries(System.out);
		System.out.println("\n\nMOVING TAXIES:");
		movingTaxies.printTimeSeries(System.out);
//		minBikes.printTimeSeries(System.out);
//		maxBikes.printTimeSeries(System.out);
//		averageBikes.printTimeSeries(System.out);
	}
	
}
