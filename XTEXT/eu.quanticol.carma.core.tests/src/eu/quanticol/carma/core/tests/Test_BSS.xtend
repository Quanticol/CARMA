package eu.quanticol.carma.core.tests

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel
import org.cmg.ml.sam.sim.sampling.StatisticSampling
import eu.quanticol.carma.simulator.CarmaSystem
import org.cmg.ml.sam.sim.SimulationEnvironment
import org.cmg.ml.sam.sim.sampling.SamplingCollection

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_BSS {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''

const ZONES = 5;
const PARKINGS = 10;
const CAPACITY = 10;
const INITIAL_AVAILABILITY = 10;
const TOTAL_USERS = 600;

const get_rate = 2; //Each parking station can release 1 bike per time units;
const ret_rate = 2; //Each parking station can receive 1 bike per time units;
const move_rate = 0.01; //The average travel time is 20 time units;
const arrival_rate = 0.5; //Every 2 time units a new user enter in the system;  

component Station( int _loc , int capacity , int available ) {
	store {
		attrib _loc := _loc;
		attrib available := available;
		attrib capacity := capacity;
	}
	
	behaviour {
		G = [my.available>0]get<>{ my.available := my.available-1; }.G;
		R = [my.available<my.capacity]ret<>{ my.available := my.available+1; }.R;
	}
	
	init {
		G|R
	}
	
}

component User( int _loc , int dest ) {
	
	store {
		attrib _loc := _loc;
		attrib dest := dest;
	}
	
	behaviour {
		P = get[ my._loc == _loc ]().B;
		B = move*[ false ]<>{ my._loc := my.dest; }.W;
		W = ret[ my._loc == _loc ]().kill;				
	}
	
	init {
		P
	}
}

component Arrival( int _loc ) {
	
	store {
		attrib _loc := _loc;		
	}
	
	behaviour {
		A = arrival*[false]<>.A;
	}
	
	init {
		A
	}
	
}

fun real ReceivingProb( int _size ) {
	if (_size != 0) {
		return 1.0/real(_size);
	} else {
		return 0.0;
	}
}

system Scenario1 {
	
	collective {
		for ( i ; i<ZONES ; 1 ) {
			for ( j ; j<PARKINGS ; 1 ) {
				new Station( i , CAPACITY, INITIAL_AVAILABILITY );
			}
			new Arrival(i);
		}
	}
	
	environment {
		store {
			attrib users := 0;
		}
		weight {
			get {
				return ReceivingProb( #{ User[P] | my._loc == sender._loc }  );
			}
			ret { 
				return ReceivingProb( #{ User[W] | my._loc == sender._loc }  );
			}
			default {
				return 1;
			}
		}
		rate {
			get { return get_rate; }
			ret { return ret_rate; }
			move* { return move_rate; }
			arrival* { return (global.users<TOTAL_USERS?arrival_rate:0.0); }
			default { return 1; }
		}
		update {
			arrival* { 
				users := global.users+1;
				new User( sender._loc , U(0,1,2,3,4) );
			}
			ret {
				users := global.users-1;
			}
		}
		
	}
	
}

measure TotalUser = global.users;
//measure Pedestrians = #{ User[P] | true };
//measure Bikers = #{ User[B] | true };
//measure Waiting = #{ User[W] | true };

measure Pedestrians( int l ) = #{ User[P] | my._loc == l };
measure Bikers( int l ) = #{ User[B] | my._loc == l };
measure Waiting( int l ) = #{ User[W] | my._loc == l };
measure MinBikes( int l ) = min{ my.available | my._loc == l };
measure MaxBikes( int l ) = max{ my.available | my._loc == l };
measure AverageBikes( int l ) = min{ my.available | my._loc == l };
measure WaitingABike( int l ) = #{ User[P] | my._loc == l };
measure WaitingASlot( int l ) = #{ User[W] | my._loc == l };
	'''
	
	@Test
	def void test_Parser(){
		code.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		code.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			val m = o as CarmaModel
			val samplings = 100
			val dt = 1
			val deadline = samplings*dt
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
//			var statistics = new StatisticSampling<CarmaSystem>(samplings+1,dt,m.getMeasure("FireTime"))
			var sim = new SimulationEnvironment( m.getFactory( "Scenario1" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
			var data = sim.timeSeries
			data.forEach[ it.printTimeSeries(System::out)]			

	]
	}
	
}