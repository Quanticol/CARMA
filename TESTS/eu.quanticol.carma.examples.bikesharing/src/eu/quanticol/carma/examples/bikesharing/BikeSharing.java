/**
 * 
 */
package eu.quanticol.carma.examples.bikesharing;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class BikeSharing extends CarmaSystem {
	
	
	public static final int ZONES = 4;
	public static final int TAKE_BIKE = 0;
	public static final int RETURN_BIKE = 1;
	public static final int MOVE = 2;
	public static final int STOP = 3;
	public static final int RESTART = 4;
	public static final double MOVE_RATE = 0.1;
	public static final double STOP_RATE = 0.1;
	public static final double RESTART_RATE = 1.0;
	public static final double TAKE_BIKE_RATE = 1.0;	
	public static final double RETURN_BIKE_RATE = 1.0;
	public static final int PEDESTRIAN = 0;
	public static final int BIKER = 1;
	

	public BikeSharing(int bikers, int pedestrians , int parking_stations , int bikes , int slots ) {
		super();
		for( int i=0 ; i<bikers ; i++ ) {
			CarmaComponent c = getBiker( i%ZONES );
			addComponent( c );
		}
		for( int i=0 ; i<pedestrians ; i++ ) {
			CarmaComponent c = getPedestrian( i%ZONES );
			addComponent( c );
		}
		for( int i=0 ; i<ZONES ; i++ ) {
			for(  int j=0 ; j<parking_stations ; j++ ) {
				addComponent(getParkingStation(i, bikes, slots));
			}
		}
	}

	private CarmaComponent getBiker( int zone ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.addAgent( new CarmaSequentialProcess(c, BikeSharingDefinitions.UserProcess , "Biker" ) );
		c.set( "status" ,  BikeSharing.BIKER );
		return c;
	}

	private CarmaComponent getPedestrian( int zone ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.addAgent( new CarmaSequentialProcess(c, BikeSharingDefinitions.UserProcess , "Pedestrian" ) );
		c.set( "status" ,  BikeSharing.PEDESTRIAN );
		return c;
	}

	private CarmaComponent getParkingStation( int zone , int bikes , int slots ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.set("slots", slots);
		c.set("bikes", bikes);
		c.addAgent(new CarmaSequentialProcess(c, BikeSharingDefinitions.ParkingProcess) );
		return c;
	}


	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return 1.0; //Broadcast input is always received
	}

	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,
			int action) {
		return 1.0; //Input are always received
	}

	@Override
	public double broadcastRate(CarmaStore sender, int action) {
		if (action == MOVE) {
			return MOVE_RATE;
		}
		if (action == STOP) {
			return STOP_RATE;
		}
		if (action == RESTART) {
			return RESTART_RATE;
		}
		return 0.0; //In this scenario broadcast is not used.
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		if (action == TAKE_BIKE) {
			return TAKE_BIKE_RATE;
		}
		if (action == RETURN_BIKE) {
			return RETURN_BIKE_RATE;					
		}
		return 0;
	}

	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action, Object value) {
	}

	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, CarmaStore receiver , int action, Object value ) {
	}
	
	public double averageAvailableBikes(int zone) {
		double toReturn = 0.0;
		double count = 0.0;
		for (CarmaComponent caspaComponent : collective) {
			Integer cZone = caspaComponent.get("zone", Integer.class);
			Integer nBikes = caspaComponent.get("bikes", Integer.class);
			if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
				count += 1.0;
				toReturn += nBikes.intValue();
			}
		}
		if (count == 0.0) {
			return 0.0;
		}
		return toReturn/count;
	}
	
	public double maxAvailableBikes(int zone) {
		int toReturn = 0;
		for (CarmaComponent caspaComponent : collective) {
			Integer cZone = caspaComponent.get("zone", Integer.class);
			Integer nBikes = caspaComponent.get("bikes", Integer.class);
			if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
				if (nBikes.intValue()>toReturn) {
					toReturn = nBikes.intValue();
				}
			}
		}
		return toReturn;
	}

	public double minAvailableBikes(int zone) {
		int toReturn = Integer.MAX_VALUE;
		for (CarmaComponent caspaComponent : collective) {
			Integer cZone = caspaComponent.get("zone", Integer.class);
			Integer nBikes = caspaComponent.get("bikes", Integer.class);
			if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
				if (nBikes.intValue()<toReturn) {
					toReturn = nBikes.intValue();
				}
			}
		}
		return toReturn;
	}
	
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new BikeSharingFactory(0,160,4,0,10)
		);
		StatisticSampling<CarmaSystem> minBikes = new StatisticSampling<CarmaSystem>(101, 1.0, new MinAvailableBikes(0) );
		StatisticSampling<CarmaSystem> maxBikes = new StatisticSampling<CarmaSystem>(101, 1.0, new MaxAvailableBikes(0) );
		StatisticSampling<CarmaSystem> averageBikes = new StatisticSampling<CarmaSystem>(101, 1.0, new AverageAvailableBikes(0) );
		system.setSampling( new SamplingCollection<CarmaSystem>( minBikes , maxBikes , averageBikes ) );
		system.simulate(200,100);
		minBikes.printTimeSeries(System.out);
		maxBikes.printTimeSeries(System.out);
		averageBikes.printTimeSeries(System.out);
	}


	public static class MinAvailableBikes implements Measure<CarmaSystem> {

		private int zone;
		
		public MinAvailableBikes( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			int toReturn = Integer.MAX_VALUE;
			for (CarmaComponent caspaComponent : t.getCollective()) {
				Integer cZone = caspaComponent.get("zone", Integer.class);
				Integer nBikes = caspaComponent.get("bikes", Integer.class);
				if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
					if (nBikes.intValue()<toReturn) {
						toReturn = nBikes.intValue();
					}
				}
			}
			return toReturn;
		}

		@Override
		public String getName() {
			return "Min Bikes";
		}
		
	}
	
	public static class MaxAvailableBikes implements Measure<CarmaSystem> {

		private int zone;
		
		public MaxAvailableBikes( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			int toReturn = 0;
			for (CarmaComponent caspaComponent : t.getCollective()) {
				Integer cZone = caspaComponent.get("zone", Integer.class);
				Integer nBikes = caspaComponent.get("bikes", Integer.class);
				if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
					if (nBikes.intValue()>toReturn) {
						toReturn = nBikes.intValue();
					}
				}
			}
			return toReturn;
		}

		@Override
		public String getName() {
			return "MX Bikes";
		}
		
	}


	public static class AverageAvailableBikes implements Measure<CarmaSystem> {

		private int zone;
		
		public AverageAvailableBikes( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			double toReturn = 0.0;
			double count = 0.0;
			for (CarmaComponent caspaComponent : t.getCollective()) {
				Integer cZone = caspaComponent.get("zone", Integer.class);
				Integer nBikes = caspaComponent.get("bikes", Integer.class);
				if ((cZone != null)&&(nBikes != null)&&(cZone.intValue()==zone)) {
					count += 1.0;
					toReturn += nBikes.intValue();
				}
			}
			if (count == 0.0) {
				return 0.0;
			}
			return toReturn/count;
		}

		@Override
		public String getName() {
			return "MX Bikes";
		}
		
	}
	
	
//	protected EvaluationContext copy_context = new EvaluationContext() {
//		
//		@Override
//		public double getRate(CaspaStore store, int action, boolean isBroadcast) {
//			Integer zone = store.get("zone", Integer.class);
//			if (zone == null) {
//				return 0;
//			}
//			double bikers = 1+getBikersAt( zone );
//			double pedestrians = 1+getPedestriansAt( zone );
//			switch (action) {
//			case MOVE:
//				return (1-(pedestrians/(bikers+pedestrians)))*MOVE_RATE;
//			case STOP:
//				return (pedestrians/(bikers+pedestrians))*STOP_RATE;
//			default:
//				return 0;
//			}
//		}
//		
//		@Override
//		public double getProbability(CaspaStore src, CaspaStore trg, int action,
//				boolean isBroadcast) {
//			return 1.0;
//		}
//		
//	};
//
//	protected EvaluationContext escape_context = new EvaluationContext() {
//		
//		@Override
//		public double getRate(CaspaStore store, int action, boolean isBroadcast) {
//			Integer zone = store.get("zone", Integer.class);
//			if (zone == null) {
//				return 0;
//			}
//			double bikers = 1+getBikersAt( zone );
//			double pedestrians = 1+getPedestriansAt( zone );
//			switch (action) {
//			case MOVE:
//				return (pedestrians/(bikers+pedestrians))*MOVE_RATE;
//			case STOP:
//				return (bikers/(bikers+pedestrians))*STOP_RATE;
//			default:
//				return 0;
//			}
//		}
//		
//		@Override
//		public double getProbability(CaspaStore src, CaspaStore trg, int action,
//				boolean isBroadcast) {
//			return 1.0;
//		}
//		
//	};
//	
//	protected EvaluationContext random_context = new EvaluationContext() {
//		
//		@Override
//		public double getRate(CaspaStore store, int action, boolean isBroadcast) {
//			switch (action) {
//			case MOVE:
//				return MOVE_RATE;
//			case STOP:
//				return STOP_RATE;
//			default:
//				return 0;
//			}
//		}
//		
//		@Override
//		public double getProbability(CaspaStore src, CaspaStore trg, int action,
//				boolean isBroadcast) {
//			return 1.0;
//		}
//		
//	};
//
//	@Override
//	protected EvaluationContext getEvaluationContext(CaspaStore store) {
//		return copy_context;
////		return escape_context;
////		return random_context;
//	}

	protected int getBikersAt(Integer zone) {
		int toReturn = 0;
		for (CarmaComponent caspaComponent : collective) {
			if (zone.equals(caspaComponent.get("zone", Integer.class))) {
				Integer status = caspaComponent.get("status", Integer.class);
				if ((status != null)&&(status == BikeSharing.BIKER)) {
					toReturn++;
				}
			}
		}
		return toReturn;
	}

	protected int getPedestriansAt(Integer zone) {
		int toReturn = 0;
		for (CarmaComponent caspaComponent : collective) {
			if (zone.equals(caspaComponent.get("zone", Integer.class))) {
				Integer status = caspaComponent.get("status", Integer.class);
				if ((status != null)&&(status == BikeSharing.PEDESTRIAN)) {
					toReturn++;
				}
			}
		}
		return toReturn;
	}

}
