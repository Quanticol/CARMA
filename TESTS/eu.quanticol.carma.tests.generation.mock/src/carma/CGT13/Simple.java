package carma.CGT13;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import eu.quanticol.carma.simulator.*;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
public class Simple extends CarmaSystem {
	
	//constructor
	public Simple(){
		addComponent(getProducer(1,1,1,1));
		addComponent(getConsumer(1,1,1,1));
		global_store.set(CGT13Definition.TRANSACTIONS_ATTRIBUTE,0);
	}
	
	//define components
	private CarmaComponent getProducer(int a,int b,int c,int d) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT13Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT13Definition.POSITION_X_ATTRIBUTE, c);
		c4rm4.set( CGT13Definition.POSITION_Y_ATTRIBUTE, d);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT13Definition.ProducerProcess,
		CGT13Definition.ProducerProcess.getState("state_Produce" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT13Definition.ProducerProcess,
		CGT13Definition.ProducerProcess.getState("state_Send" )));
		return c4rm4;
	}
	private CarmaComponent getConsumer(int a,int b, int x, int y) {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT13Definition.PRODUCT_ATTRIBUTE, a);
		c4rm4.set( CGT13Definition.POSITION_X_ATTRIBUTE, x);
		c4rm4.set( CGT13Definition.POSITION_Y_ATTRIBUTE, y);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT13Definition.ConsumerProcess,
		CGT13Definition.ConsumerProcess.getState("state_Consume" )));
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT13Definition.ConsumerProcess,
		CGT13Definition.ConsumerProcess.getState("state_Receive" )));
		return c4rm4;
	}
	private CarmaComponent getChild() {
		CarmaComponent c4rm4 = new CarmaComponent();
		c4rm4.set( CGT13Definition.ME_ATTRIBUTE, 0);
		c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
		CGT13Definition.ChildProcess,
		CGT13Definition.ChildProcess.getState("state_Nothing" )));
		return c4rm4;
	}
	
	//predicates
	
	
	
	
	
	
	//evol rules
	@Override
	public double broadcastRate(CarmaStore sender, int action){
		if (action == CGT13Definition.PRODUCE
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double unicastRate(CarmaStore sender, int action){
		if (action == CGT13Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1;
		}
		return 1.0;
	}
	
	@Override
	public double broadcastProbability(CarmaStore sender, CarmaStore receiver,int action){
		return 1.0;
	}
	
	@Override
	public double unicastProbability(CarmaStore sender, CarmaStore receiver,int action){
		if (action == CGT13Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				return 1 / 1.0; //getMeasure162929249__All().measure(t);
		}
		return 1.0;
	}
	
	@Override
	public void broadcastUpdate(RandomGenerator r , CarmaStore sender, int action, Object value){
	}
	
	@Override
	public void unicastUpdate(RandomGenerator r , CarmaStore sender, CarmaStore receiver, int action, Object value){
		if (action == CGT13Definition.SEND
		&& CarmaPredicate.TRUE.satisfy(sender)
		) {
				addComponent(getChild());
				boolean hasAttributes = true;
				int transactions = 0;
				if(global_store.get("transactions" , Integer.class) != null){
					transactions = global_store.get("transactions" , Integer.class); 
				} else { 
					hasAttributes = false;
				}
				if(hasAttributes){
					global_store.set("transactions",transactions + 1);
				}
		}
	}
	
	//measures
	public static CarmaProcessPredicate getMeasure162929249__All_State_Predicate(){
		return new CarmaProcessPredicate() {
			
			@Override
			public boolean eval(CarmaProcess p) {
				return ( 
				(((CarmaSequentialProcess) p).automaton() ==  CGT13Definition.ChildProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Nothing") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  CGT13Definition.ProducerProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Send") != null ) ||
				(((CarmaSequentialProcess) p).automaton().getState("state_Produce") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  CGT13Definition.ConsumerProcess) && (
				(((CarmaSequentialProcess) p).automaton().getState("state_Receive") != null ) ||
				(((CarmaSequentialProcess) p).automaton().getState("state_Consume") != null ) ||
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				;
			}
		};
	}
	protected static CarmaPredicate getPredicateMeasure162929249__All() {
		return new CarmaPredicate() {
			@Override
			public boolean satisfy(CarmaStore store) {
				boolean hasAttributes = true;
				if(hasAttributes)
					return true;
				else
					return false;
			}
		};
	}
	
	
	public static ComponentPredicate getMeasure162929249__All_BooleanExpression_Predicate(){
		return new ComponentPredicate() {
			
			@Override
			public boolean eval(CarmaComponent c){
				return getPredicateMeasure162929249__All().satisfy(c.getStore()) && (c.isRunning(getMeasure162929249__All_State_Predicate()));
			}
		};
	}
	//getMethod
	public static Measure<CarmaSystem> getMeasure162929249__All(){
		
		return new Measure<CarmaSystem>(){
		
			ComponentPredicate predicate = getMeasure162929249__All_BooleanExpression_Predicate();
		
			@Override
			public double measure(CarmaSystem t){
				//TODO
			
				return t.measure(predicate);
		
			};
		
			@Override
			public String getName() {
				return "Measure162929249__All";
			}
		};
	}
	
	//main
	/*MAIN*/
	public static void main( String[] argv ) {
		SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
			new CGT13Factory()
		);
	
		int deadline = 50;
		
		SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
		
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, CGT13Definition.getMeasureWaiting__All(1)));
		
		system.setSampling(sc);
		system.simulate(100,50);
		for(int i = 0; i < sc.size(); i++){
			((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
		}
	}
	
}
