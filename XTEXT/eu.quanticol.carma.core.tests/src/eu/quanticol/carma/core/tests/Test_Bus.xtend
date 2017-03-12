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
class Test_Bus {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
const STANDAR_RATE_ROUTE_1 = 1.0/15.0;
const CONGESTION_RATE_ROUTE_1 = 1.0/20.0;

const STANDAR_RATE_ROUTE_2 = 1.0/7.5;
const CONGESTION_RATE_ROUTE_2 = 1.0/15.0;

const TO_GARAGE_RATE = 1/360;
const MAINTAINANCE_RATE = 1/60;

const CONGESTION_RATE = 1/120;
const DECONGESTION_RATE = 1/30;

const LEAVE_RATE = 1.0;
const QUEUE_RATE = 1.0;

const SIZE = 8;
const SIZE_ROUTE_1 = 1;
const SIZE_ROUTE_2 = 0;
const START_ROUTE_1 = 0;
const START_ROUTE_2 = 4;

space BusRoute(int stops) {
	universe <int zone>
	nodes { 
		[ -1 ]; //Initial bus location
		for i from 0 to stops {
			[ i ];			
		}
	}
	connections {
		//route 1
		for i from 0 to 8 {
			[i] -> [(i+1)%8] {route=1};			
		}
		
		//route 2
		for i from 0 to (stops/2) {
			[2*i] -> [(2*(i+1))%8] {route=2};
		}
	}	
}

space BusRoute2(int stops) {
	universe <int zone>
	nodes { 
		[ -1 ]; //Initial bus location
		for i from 0 to stops {
			[ i ];			
		}
	}
	connections {
		//route 1
		for i from 0 to 8 {
			[i] -> [(i+1)%8] {route=1};			
		}
		
		//route 2
		for i from 0 to (stops/2) {
			[2*i] -> [(2*(i+1))%8] {route=2};
		}
	}	
}

fun location nextDestination( int route , location current ) {
	return current.outgoing().filter( 
				@.route == route
			).map(
				@.target	
			).select( 1.0 );
}
//fun location nextDestination( int route , location current ) {
//	if (route == 1) {
//		return [ ((current.zone+1)%8) ];
//	} else {
//		return [ ((current.zone+2)%8) ];
//	}
//}


component Bus(int number){
	store{
		attrib route := number;
		attrib location next := none;
		attrib queuepos := 0;
		attrib end = -1;
	}
	 
	behaviour{
		G = arrive*[true](x,y) {
			loc = x;
			end = y;
		}.S;
		W = [queuepos==1]leave*[loc == my.loc](x){ 
			next = nextDestination(my.route,loc);
		}.T
		+ [queuepos>1]leave*[loc == my.loc](x){
			queuepos = queuepos-1;
		}.W;
		T = [loc.zone != end ]move*[true]<loc,next>{
			loc = next;
			next = none;
		}.S
		+ [loc.zone == end]maintainance*{
			loc = [ -1 ];
			next = none;
		}.G;
		S = enter<>.Q;
 		Q = queueorder*[loc==my.loc](x){ 
 			queuepos = x;
 		}.W;
	}
	
	init{
		G
	}
}

component Stop( ){
	store{
		attrib buses := 0;
		attrib congestion = false;
	}
	
	behaviour{
		S = [buses>0]leave*[loc == my.loc]<loc>{buses := buses - 1;}.S
		+ enter[my.loc == loc](){buses := buses + 1;}.A;
//		+ [ !congestion ]congestion*{ congestion = true; }.S
//		+ [ congestion ]resolved*{ congestion = false; }.S;
		A = queueorder*[loc == my.loc]<buses>.S;
	}
	
	init{
		S
	}
}

component Arrival(int route, int end, real r) {
	
	store {
		attrib r = r;
		attrib route = route;
		attrib end = end;
	}
	
	behaviour {
		A = arrive*[my.route==route]<loc,end>.A;	
	}
	
	init {
		A
	}
	
}

measure AllBusses = #{ Bus[*] | true };
measure AllW = #{ Bus[W] | true };
measure AllT = #{ Bus[T] | true };
measure AllQ = #{ Bus[Q] | true };
measure AllG = #{ Bus[G] | true };

measure TravelingAt( int i ) = #{Bus[T] | my.next == [ i ]};
measure QueuingWaitingAt( int i ) = #{Bus[Q] | my.loc == [ i ] };
measure WaitingAt( int i ) = #{Bus[W] | my.loc == [ i ]};
measure BusAtGarage = #{ Bus[G] | true };
measure LostBusses = #{ Bus[*] | my.loc == none };

system ScenarioTest1{
	space BusRoute(SIZE)
	collective{
		for l in locations {
			new Stop()@l;
		}	
		new Arrival( 1 , 1, 1.0/5.0 )@[ 0 ];
		new Arrival( 2 , 2, 1.0/5.0 )@[ 4 ];
		new Bus(1)@[-1]<SIZE_ROUTE_1>;
		new Bus(2)@[-1]<SIZE_ROUTE_2>;
	}
	
	environment{
		store{
			attrib congestioned = 0;
		}
		
		prob{
			default{ return 1.0; }
		}
		
		weight {
			default {
				return 1.0;
			}
		}
		
		rate{
			arrive*{ 
				if (sender.route==1) {
					return STANDAR_RATE_ROUTE_1;
				} else {
					return STANDAR_RATE_ROUTE_2;
				}
			}
			leave* {
				return LEAVE_RATE;
			}
//			togarage* {
//				return TO_GARAGE_RATE;
//			}
			maintainance* {
				return MAINTAINANCE_RATE;
			}
			move* {
				if (sender.route == 1) {
					return STANDAR_RATE_ROUTE_1;
				} else {
					return STANDAR_RATE_ROUTE_2;
				}
			}
			enter {
				return 2.0;
			}
			queueorder* {
				return QUEUE_RATE;
			}
			default{return 1000.0;}			
		}
		
		update{
		}
	}
}




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
//			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
////			var statistics = new StatisticSampling<CarmaSystem>(samplings+1,dt,m.getMeasure("FireTime"))
			var sim = new SimulationEnvironment( m.getFactory( "ScenarioTest1" ) )
//			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
	]
	}
	
}