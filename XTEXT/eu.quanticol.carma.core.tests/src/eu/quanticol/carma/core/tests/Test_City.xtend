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
class Test_City {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
component QueuePoint(int x, int y, int i, int c, int s, process Z){
	store {
		attrib location_x := x;
		attrib location_y := y;
		attrib id := i;
		
		attrib capacity := c;
		attrib _size := s;
		
	}
	behaviour{

		EMPTY = oQiP_addPerson<my.location_x, my.location_y, my.id>{_size := my._size +1; }.FILLED;

		FILLED = oQiP_addPerson[_size < capacity - 1]<my.location_x, my.location_y, my.id>{_size := my._size +1; }.FILLED
				+ oQiP_addPerson[my._size == my.capacity - 1]<my.location_x, my.location_y, my.id>{_size := capacity; }.FILLED;
    }

    init{
        Z
    }
    
}

component Person(int x, int y, int i, int q, process Z){
	store {
		attrib location_x := x;
		attrib location_y := y;
		attrib id := i;
		attrib queue := q;
	}
	behaviour{
		FREE = oQiP_addPerson[qLocation_x == my.location_x && qLocation_x == my.location_x]
		                (qLocation_x, qLocation_y, q_id){queue := q_id; }.FREE;		
        }

    init{
        Z
    }
}

measure Free = #{Person[FREE]| true };
measure Filled = #{QueuePoint[FILLED]| true };

system SmallCity{

    collective{
    	
    	new Person(1, 1, 1, 0, FREE);
    	new Person(5, 5, 2, 0, FREE);
    	new Person(6, 6, 3, 0, FREE);

        new QueuePoint(5, 5, 1, 3, 0, EMPTY);
        new QueuePoint(6, 6, 2, 3, 0, EMPTY);
		
    }

    environment{

        store{
        }

        prob{
			default { return 1.0; }
        }

        rate{

			default { return 1.0; }
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
			val samplings = 1000
			val deadline = 100.0
			val dt = deadline/samplings
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var sim = new SimulationEnvironment( m.getFactory( "SmallCity" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
		]
	}
	
}