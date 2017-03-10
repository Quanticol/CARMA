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
class Test_UseOfEnum {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
	
enum Types=PROVIDER,DELEGATE;
const R = 10.0;

component Server(){
    store{
        attrib role := PROVIDER;
    }
    behaviour{
        Q = [role==PROVIDER]engage*.kill;
    }
    init{
       Q
    }
}

system Actual{

    collective{
    	for(i; i < 10 ; i + 1){
    		new Server();
    	}
    }

    environment{
    	
    	
    	prob{
              default { return 1.0; }
    	}
    	
        rate{
        	engage* {
        		if (sender.role==PROVIDER) {
        			return 2.0*R;
        		}
        		if (sender.role==DELEGATE) {
        			return R;
        		}
        		return 0.0;
        	}
        }
        
        update{

        }
    }    }
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
		]
	}
	
}