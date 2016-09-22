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
import eu.quanticol.carma.core.carma.CarmaPackage
import eu.quanticol.carma.core.validation.CARMAValidator

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_Bed {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
component Gen_Agent(){
	store {}
	behaviour {G  = a*[true](n).G1;
		       G1 = b*[true](n).G2;
		       G2 = c*[true](n).nil;
	}
    init{G}	
}

component Test_Agent(){
	store{}
	behaviour{T = run*[false]<>.nil;}
	init{T}
}

component Comm_Agent(){
	store{}
	behaviour{C = a*[true]<>.C + b*[true]<>.C + c*[true]<>.C;}
	init{C}
}

measure TA    = 1 + 0.5 * #{Test_Agent[*]|true};
measure TA_T  = 2 + 0.5 * #{Test_Agent[T]|true};

measure GA    = 3 + 0.5 * #{Gen_Agent[*]|true};
measure GA_G  = 4 + 0.5 * #{Gen_Agent[G]|true};
measure GA_G1 = 5 + 0.5 * #{Gen_Agent[G1]|true};
measure GA_G2 = 6 + 0.5 * #{Gen_Agent[G2]|true};


system Sys{
    collective{
    	new Gen_Agent();
    	new Comm_Agent();
    }
    environment{
        store{}
        prob{default : 1.0;}
        rate{
        	[true] a* : 100.0;
        	default : 0.1;
        }
		update{
			//[true] gen* : new Test_Agent();
		}
    }
}




	'''
	
	@Test
	def void test_Parser(){
		code.parse.assertError(CarmaPackage::eINSTANCE.inputAction,CARMAValidator::ERROR_Wrong_Number_Of_Parameters_Input,"")
	}

//	@Test
//	def void test_Compiler(){
//		code.compile[ 
//			var o = getCompiledClass.newInstance 
//			assertNotNull( o )
//			assertTrue( o instanceof CarmaModel )
//			val m = o as CarmaModel
//			val samplings = 1000
//			val deadline = 100.0
//			val dt = deadline/samplings
//			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
//			var sim = new SimulationEnvironment( m.getFactory( "Sys" ) )
//			sim.sampling  = new SamplingCollection( statistics )
//
//			sim.simulate(deadline)
//		]
//	}
	
}