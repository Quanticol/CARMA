/**
 * 
 */
package eu.quanticol.SIRS;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.sampling.Measure;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaSequentialProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author cdw
 *
 */
public class SIRS extends CarmaSystem {
	
	public static final int ZONES = 4;
	
	//actions
	public static final int CONTACT = 0;
	public static final int RECOVERY = 1;
	public static final int SUSCEPTIBLE = 2;
	public static final int MOVE = 3;
	
	//rates
	public static final double CONTACT_RATE = 0.1;
	public static final double RECOVERY_RATE = 0.1;
	public static final double SUSCEPTIBLE_RATE = 1.0;
	public static final double MOVE_RATE = 1.0;
	
	//components
	public static final int SUSCEPTIBLES = 0;
	public static final int RECOVEREDS = 1;
	public static final int INFECTIVES = 2;
	

	public SIRS(int susceptibles , int infectives, int recovereds , int zones) {
		super();
		for( int i=0 ; i<susceptibles ; i++ ) {
			CarmaComponent c = getSusceptibles( i%ZONES );
			addComponent( c );
		}
		
		addComponent(getInfectives( 1 ));
		
		addComponent(getRecovereds( 1 ));
	}

	private CarmaComponent getSusceptibles( int zone ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.addAgent( new CarmaSequentialProcess(c, SIRSDefinitions.SIRProcess , "Susceptible" ) );
		c.set( "status" ,  SIRS.SUSCEPTIBLE );
		return c;
	}
	
	private CarmaComponent getInfectives( int zone ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.addAgent( new CarmaSequentialProcess(c, SIRSDefinitions.SIRProcess , "Infective" ) );
		c.set( "status" ,  SIRS.INFECTIVES );
		return c;
	}
	
	private CarmaComponent getRecovereds( int zone ) {
		CarmaComponent c = new CarmaComponent();
		c.set("zone", zone );
		c.addAgent( new CarmaSequentialProcess(c, SIRSDefinitions.SIRProcess , "Recovered" ) );
		c.set( "status" ,  SIRS.RECOVEREDS );
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
		if (action == CONTACT) {
			return CONTACT_RATE;
		}
		if (action == SUSCEPTIBLE) {
			return SUSCEPTIBLE_RATE;
		}
		if (action == RECOVERY){
			return RECOVERY_RATE;
		}
		return 0.0; //In this scenario broadcast is not used.
	}

	@Override
	public double unicastRate(CarmaStore sender, int action) {
		return 0;
	}

	public static class SusceptiblesMeasure implements Measure<CarmaSystem> {

		private int zone;
		
		public SusceptiblesMeasure( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			int toReturn = 0;
			for (CarmaComponent carmaComponent : t.getCollective()) {
				if (((Integer) zone).equals(carmaComponent.get("zone", Integer.class))) {
					Integer status = carmaComponent.get("status", Integer.class);
					if ((status != null)&&(status == SIRS.SUSCEPTIBLES)) {
						toReturn++;
					}
				}
			}
			return toReturn;
		}

		@Override
		public String getName() {
			return "Susceptibles";
		}
		
	}
	
	public static class InfectivesMeasure implements Measure<CarmaSystem> {

		private int zone;
		
		public InfectivesMeasure( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			int toReturn = 0;
			for (CarmaComponent carmaComponent : t.getCollective()) {
				if (((Integer) zone).equals(carmaComponent.get("zone", Integer.class))) {
					Integer status = carmaComponent.get("status", Integer.class);
					if ((status != null)&&(status == SIRS.INFECTIVES)) {
						toReturn++;
					}
				}
			}
			return toReturn;
		}

		@Override
		public String getName() {
			return "Infectives";
		}
		
	}


	public static class RecoveredsMeasure implements Measure<CarmaSystem> {

		private int zone;
		
		public RecoveredsMeasure( int zone ) {
			this.zone = zone;
		}
		
		@Override
		public double measure(CarmaSystem t) {
			int toReturn = 0;
			for (CarmaComponent carmaComponent : t.getCollective()) {
				if (((Integer) zone).equals(carmaComponent.get("zone", Integer.class))) {
					Integer status = carmaComponent.get("status", Integer.class);
					if ((status != null)&&(status == SIRS.RECOVEREDS)) {
						toReturn++;
					}
				}
			}
			return toReturn;
		}

		@Override
		public String getName() {
			return "Recovereds";
		}
		
	}
	
	

	protected int getSusceptiblesAt(Integer zone) {
		int toReturn = 0;
		for (CarmaComponent carmaComponent : collective) {
			if (zone.equals(carmaComponent.get("zone", Integer.class))) {
				Integer status = carmaComponent.get("status", Integer.class);
				if ((status != null)&&(status == SIRS.SUSCEPTIBLES)) {
					toReturn++;
				}
			}
		}
		return toReturn;
	}

	protected int getInfectivesAt(Integer zone) {
		int toReturn = 0;
		for (CarmaComponent carmaComponent : collective) {
			if (zone.equals(carmaComponent.get("zone", Integer.class))) {
				Integer status = carmaComponent.get("status", Integer.class);
				if ((status != null)&&(status == SIRS.INFECTIVES)) {
					toReturn++;
				}
			}
		}
		return toReturn;
	}
	
	protected int getRecoveredsAt(Integer zone) {
		int toReturn = 0;
		for (CarmaComponent carmaComponent : collective) {
			if (zone.equals(carmaComponent.get("zone", Integer.class))) {
				Integer status = carmaComponent.get("status", Integer.class);
				if ((status != null)&&(status == SIRS.RECOVEREDS)) {
					toReturn++;
				}
			}
		}
		return toReturn;
	}

	@Override
	public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
			int action) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void unicastUpdate(RandomGenerator random, CarmaStore sender,CarmaStore receiver, 
			int action) {
		// TODO Auto-generated method stub
		
	}

}
