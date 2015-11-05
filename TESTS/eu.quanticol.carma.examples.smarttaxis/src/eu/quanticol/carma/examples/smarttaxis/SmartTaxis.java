package eu.quanticol.carma.examples.smarttaxis;


import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.examples.smarttaxis.SmartTaxisDefinitions.InfoClass;
import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaProcessPredicate;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.simulator.ComponentPredicate;

public class SmartTaxis extends CarmaSystem {
	
	
	public SmartTaxis( int taxis , int users ) {
		
				
		for( int l=0 ; l<SmartTaxisDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
			for( int i=0 ; i<taxis ; i++ ) {
				CarmaComponent taxi = getTaxi( l );
				addComponent(taxi);
			}
		}
		
		for( int l=0 ; l<SmartTaxisDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
			CarmaComponent arrival = getArrivalComponent( l );
			addComponent( arrival );
		}
		
//		for( int l=0 ; l<SmartTaxisDefinitions.NUMBER_OF_LOCATIONS ; l++ ) {
//			for( int i=0 ; i<users ; i++ ) {
//				CaspaComponent user = getUserComponent( l );
//				addComponent( user );
//			}
//		}
		
	}

	private CarmaComponent getArrivalComponent(int l) {
		CarmaComponent c = new CarmaComponent();
		c.set( SmartTaxisDefinitions.LOC_ATTRIBUTE ,  l );
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxisDefinitions.ArrivalProcess ) );
		return c;
	}

	private CarmaComponent getTaxi(int loc) {
		CarmaComponent c = new CarmaComponent();
		c.set( SmartTaxisDefinitions.LOC_ATTRIBUTE ,  loc );
		c.set( SmartTaxisDefinitions.INFO_ATTRIBUTE ,  new SmartTaxisDefinitions.InfoClass( SmartTaxisDefinitions.NUMBER_OF_LOCATIONS ) );
		c.set( SmartTaxisDefinitions.DEST_ATTRIBUTE ,  -1 );
		c.set( SmartTaxisDefinitions.OCCUPIED_ATTRIBUTE, false);
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxisDefinitions.TaxiProcess ) );
//		c.addAgent( new CaspaSequentialProcess(c, SmartTaxisDefinitions.InfoProcess ) );
		return c;
	}

	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return SmartTaxisDefinitions.P_LOST;
	}

	private double getMoveTime(double now, Integer from, Integer to) {
		return SmartTaxisDefinitions.STEP_RATE*SmartTaxisDefinitions.steps(from,to);
	}

	@Override
	public double unicastProbability(final CarmaStore sender, CarmaStore receiver,
			int action) {
		if (action == SmartTaxisDefinitions.TAKE) {
			double foo = measure(new ComponentPredicate() {

				@Override
				public boolean eval(CarmaComponent c) {
					int loc = sender.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE);
					if (c.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE).equals(loc)) {
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
		if (action == SmartTaxisDefinitions.CALL) {
			return 1.0;
		}
		return 0;
	}

	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		if (action == SmartTaxisDefinitions.CALL) {
			return SmartTaxisDefinitions.CALL_RATE;
		}
		if (action == SmartTaxisDefinitions.EXCH) {
			SmartTaxisDefinitions.InfoClass info = sender.get(SmartTaxisDefinitions.INFO_ATTRIBUTE, SmartTaxisDefinitions.INFO_ATTRIBUTE_TYPE);
			if ((info == null)||(info.elements()==0)) {
				return 0.0;
			} else {
				return SmartTaxisDefinitions.EXCH_RATE;
			}
		}
		if (action == SmartTaxisDefinitions.CHANGE) {
			InfoClass ic = sender.get(SmartTaxisDefinitions.INFO_ATTRIBUTE, SmartTaxisDefinitions.INFO_ATTRIBUTE_TYPE);
			int loc = sender.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE);
			return SmartTaxisDefinitions.CHANGE_RATE*ic.getMovingProbability( loc );
		}
		if (action == SmartTaxisDefinitions.MOVE) {
			return getMoveTime( 
				now() , 
				sender.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE ) ,
				sender.get(SmartTaxisDefinitions.DEST_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE ) 
			);
		}
		if (action == SmartTaxisDefinitions.AGE) {
			SmartTaxisDefinitions.InfoClass info = sender.get(SmartTaxisDefinitions.INFO_ATTRIBUTE, SmartTaxisDefinitions.INFO_ATTRIBUTE_TYPE);
			if ((info == null)||(info.elements()==0)) {
				return 0.0;
			} else {
				return SmartTaxisDefinitions.AGE_RATE;
			}
		}
		if (action == SmartTaxisDefinitions.ARRIVE) {
			return SmartTaxisDefinitions.arrivalRate( now() , sender.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE ) );
		}
		return 0;
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (action == SmartTaxisDefinitions.TAKE) {
			return SmartTaxisDefinitions.TAKE_RATE;
		}
		if (action == SmartTaxisDefinitions.CALL) {
			return SmartTaxisDefinitions.CALL_RATE;
		}
		return 0;
	}

	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action, Object value) {
		if (action == SmartTaxisDefinitions.ARRIVE) {
			int loc = sender.get(SmartTaxisDefinitions.LOC_ATTRIBUTE, SmartTaxisDefinitions.LOC_ATTRIBUTE_TYPE);
			int dest = SmartTaxisDefinitions.getDestination( now , loc , r );
			addComponent( 
					getUserComponent( 
							loc ,
							dest//r.nextInt(SmartTaxisDefinitions.NUMBER_OF_LOCATIONS)
					));
		}
	}

	private CarmaComponent getUserComponent(int loc , int dest ) {
		CarmaComponent c = new CarmaComponent();
		c.set(SmartTaxisDefinitions.LOC_ATTRIBUTE, loc);
		c.set(SmartTaxisDefinitions.DEST_ATTRIBUTE, dest);
		c.addAgent( new CarmaSequentialProcess(c, SmartTaxisDefinitions.UserProcess) );
		return c;
	}

	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, CarmaStore receiver, int action, Object value) {

	}

	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new SmartTaxisFactory(SmartTaxisDefinitions.TAXIS, 0)
		);
		int deadline = (int) SmartTaxisDefinitions.LIMIT;
		StatisticSampling<CarmaSystem> waitingUsers0 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxisDefinitions.getMeasureOfWaitingUsers(SmartTaxisDefinitions.getLocId(1, 1)));
		StatisticSampling<CarmaSystem> freeTaxis0 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxisDefinitions.getMeasureOfAvailabelTaxis(SmartTaxisDefinitions.getLocId(1, 1)));
		StatisticSampling<CarmaSystem> movingTaxis = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxisDefinitions.getMeasureOfMovingTaxis());
		StatisticSampling<CarmaSystem> waitingUsers1 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxisDefinitions.getMeasureOfWaitingUsers(SmartTaxisDefinitions.getLocId(0, 0)));
		StatisticSampling<CarmaSystem> freeTaxis1 = 
				new StatisticSampling<CarmaSystem>(deadline+1, 1.0, SmartTaxisDefinitions.getMeasureOfAvailabelTaxis(SmartTaxisDefinitions.getLocId(0, 0)));
		system.setSampling( new SamplingCollection<CarmaSystem>( waitingUsers0 , waitingUsers1 ,movingTaxis, freeTaxis0 , freeTaxis1 ) );
		system.simulate(200,deadline);
		System.out.println("WAITING AT (1,1):");
		waitingUsers0.printTimeSeries(System.out);
		System.out.println("\n\nWAITING AT (0,0):");
		waitingUsers1.printTimeSeries(System.out);
		System.out.println("FREE AT (1,1):");
		freeTaxis0.printTimeSeries(System.out);
		System.out.println("FREE AT (1,1):");
		freeTaxis1.printTimeSeries(System.out);
		System.out.println("\n\nMOVING TAXIS:");
		movingTaxis.printTimeSeries(System.out);
//		minBikes.printTimeSeries(System.out);
//		maxBikes.printTimeSeries(System.out);
//		averageBikes.printTimeSeries(System.out);
	}
	
}
