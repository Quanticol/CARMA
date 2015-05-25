package carma.producer;

import org.apache.commons.math3.random.RandomGenerator;

import eu.quanticol.carma.simulator.CarmaOutput;
import eu.quanticol.carma.simulator.CarmaPredicate;
import eu.quanticol.carma.simulator.CarmaProcessAutomaton;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaStoreUpdate;

public class ProducerDefinition {
	
	
	/*METHOD VARIABLES*/
	
	
	/*COMPONENT ATTRIBUTES*/
	public static final String PRODUCT_ATTRIBUTE = "product";
	public static final Class<Integer> PRODUCT_ATTRIBUTE_TYPE = Integer.class;
	
	/*INPUT ARGUMENTS*/
	
	
	/*ENVIRONMENT ATTRIBUTES*/
	
	
	/*ACTION*/
	public static final int PRODUCE = 0;
	
	
	/*RATES*/
	public static final double PRODUCE_RATE = 1;
	
	
	/*ProcessAutomaton*/
	//create#COMPONENTNAME#Process
	public static final CarmaProcessAutomaton ProducerProcess = createProducerProcess();
	
	private static CarmaProcessAutomaton createProducerProcess() {
		
		//create new automaton
		//CarmaProcessAutomaton("#COMPONENTNAME#")
		CarmaProcessAutomaton toReturn = new CarmaProcessAutomaton("Producer");
		
		//create a state in the automata 
		//statePROCESSNAME/PROCESSREFERNCE
		CarmaProcessAutomaton.State stateProduce = toReturn.newState("Produce");
		
		CarmaOutput produceAction = new CarmaOutput( PRODUCE , true ) {

			@Override
			protected CarmaPredicate getPredicate(CarmaStore store) {
				return CarmaPredicate.FALSE;
			}

			@Override
			protected CarmaStoreUpdate getUpdate() {
				return new CarmaStoreUpdate() {
					
					@Override
					public void update(RandomGenerator r, CarmaStore store) {
						
					}
				};
			}

			@Override
			protected Object getValue(CarmaStore store) {
				// TODO Auto-generated method stub
				return null;
			}
			
		};
		
		toReturn.addTransition(stateProduce, produceAction, stateProduce);
		
		return toReturn;
	}
	
	/*MEASURES*/
	//none
	
	
	

}
