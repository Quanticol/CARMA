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
import java.util.HashSet
import org.eclipse.xtext.xbase.compiler.GeneratorConfig

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_Space {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension MyCompilationTestHelper
	
	CharSequence code = 	'''
space grid( int width , int height ) {
   universe <int x, int y>
   nodes {
   	for r from 0 to width {
   		for c from 0 to height {
   			[ r , c ];	
   		}	
   	}
   }
   connections {
      [?x,?y]: x<width-1 <-> [x+1,y] : w=1.0;
      [?x,?y]: y<height-1 <-> [x,y+1]: w=1.0;
      [?x,?y]: x<width-1 && y<height-1 <-> [x+1,x+1]: w=1.0;
   }
   areas {
      corner {
      	 [0,0]; 
      	 [width-1,0];
      	 [0,height-1];
      	 [width-1,height-1];
      }
      diagonal {
      	[?z,?w]: z==w;
      }
      border {
       [?x,?y]: x==0||x==width-1||y==0||y==height-1;
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
		]
	}
	
	
}