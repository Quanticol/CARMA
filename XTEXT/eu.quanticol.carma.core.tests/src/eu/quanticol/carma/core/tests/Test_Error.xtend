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
import eu.quanticol.carma.core.carma.UntypedVariable
import eu.quanticol.carma.core.validation.CARMAValidator

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_Error {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
//This gives an "See error log for details" error
//The error log shows
//org.eclipse.xtext.builder.BuilderParticipant  - Error during compilation of 'platform:/resource/Testbed/src/model/test_error.carma'.
//java.lang.ArrayIndexOutOfBoundsException: ......
//
// The action in A2_Agent is not a broadcast action and changing it to a broadcast action removes the error
// The error seems to occur only when there are two or more arguments passed.
//
// It seems it would be better that the model doesn't work rather than xtext fails.
// It was a real hard error to track down and explain
//

component A1_Agent(){
	store {}
	behaviour {A1 = a[true](b,b).nil;
	}
	init{A1}
}

component A2_Agent(){
	store {}
	behaviour {A2 = a*[true](b,b).nil;
	}
	init{A2}
}

component A3_Agent(){
	store {}
	behaviour {A3 = a*[true]<1,1>.nil;
	}
	init{A3}
}

system Sys{
    collective{
    	new A1_Agent();
    	new A2_Agent();
   }
    environment{
        store{}
        prob{default : 1.0;}
        rate{default : 100;}
		update{}
    }
}


	'''
	
	@Test
	def void test_Parser(){
		//code.parse.assertError(null,"")
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