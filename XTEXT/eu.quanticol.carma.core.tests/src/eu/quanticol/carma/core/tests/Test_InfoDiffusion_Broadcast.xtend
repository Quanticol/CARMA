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
import eu.quanticol.carma.simulator.CarmaSystem
import org.cmg.ml.sam.sim.sampling.StatisticSampling
import org.cmg.ml.sam.sim.sampling.SamplingCollection
import org.cmg.ml.sam.sim.SimulationEnvironment
import org.cmg.ml.sam.sim.sampling.SamplingFunction
import eu.quanticol.carma.simulator.CarmaPredicate
import eu.quanticol.carma.simulator.CarmaStore

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_InfoDiffusion {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
const N = 2;

record Message = [ location locSender ];

fun list<location> createSendingList( set<location> next ) {
	list<location> toReturn = newList(location);
	for l in next {
		toReturn = toReturn + [: l :];
	}
	return toReturn;
}

space Grid(int width, int height) {
	universe <int x,int y>
	nodes {
		for i from 0 to width {
			for j from 0 to height {
				[i,j];
			}
		}
	}
	connections {
		for i from 0 to width {
			for j from 0 to height {
				if (i<width-1) {
					[i,j] <-> [i+1,j];
				}
				if (j<height-1) {
					[i,j] <-> [i,j+1];
				}
			}
		}
	}
}

component Agent() {
	store {
		attrib list<location> pending = none;
		attrib done = false;
	}
	behaviour {
		IDLE = forward[ loc in my.loc.pre ](){ 
			my.pending = createSendingList(my.loc.post);
		}.ACTIVE
		+ 
		activate*{ 
			my.pending = createSendingList(my.loc.post);
		}.ACTIVE
		;
		ACTIVE = 
			forward[ loc in my.loc.pre ](){ 
				my.pending = createSendingList(my.loc.post);
			}.ACTIVE
			+
			[ size(pending) == 0] done*{ 
				my.done = true;
			}.nil
			+
			[ size(pending)>0 ] forward[ loc == my.head(pending) ]<> {
				my.pending = tail(my.pending);
			}.ACTIVE;
	}
	init {
		IDLE
	}
}

system WithGrid {
	space Grid( 2 , 1 )
	collective {
		for l in locations {
			new Agent()@l;
		}	
	}
	environment {
		store {
			attrib int messages = 0;
			attrib bool activated = false;
		}
		rate {
			activate* {
				if (!global.activated) {
					return 1.0; 
				} else {
					return 0.0;
				}
			}
			default {
				return 1.0;
			}
		}
		update {
			forward {
				messages = global.messages+1;
			}
			activate* {
				activated = true;
			}
		}
	}
}

measure active = #{ Agent[ACTIVE] | true };
measure idle = #{ Agent[IDLE] | true };
measure done = #{ * | my.done==true };
measure messages = global.messages;
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
			val samplings = 1000
			val dt = 1
			val deadline = samplings*dt
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var sim = new SimulationEnvironment( m.getFactory( "WithGrid" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
		]
	}
	
}