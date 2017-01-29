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
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions
import eu.quanticol.carma.simulator.space.SpaceModel
import eu.quanticol.carma.simulator.space.Tuple

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestSpace {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension MyCompilationTestHelper
	@Inject extension ReflectExtensions
	
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
   	  for r from 0 to width-1 {
		 for c from 0 to height-1 {
		 	[r,c] <-> [r+1,c]{ w = 1 };
		 	[r,c] <-> [r,c+1]{ w = 1 };	
		 }  	  	
   	  }
   }
   areas {
      corner {
      	 [0,0]; 
      	 [width-1,0];
      	 [0,height-1];
      	 [width-1,height-1];
      }
      diagonal {
      	for i from 0 to min(height, width) {
      		[i,i];	
      	}
      }
      border {
	   	  for r from 0 to width {
		 	[r,0];
			[r,height-1];
	   	  }
	   	  for c from 0 to height {
	   	  	[0,c];
	   	  	[width-1,c];
	   	  }
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
			var o2 = o.invoke("get_SPACE_grid",5,5)
			assertNotNull(o2)
			assertTrue( o2 instanceof SpaceModel)
			var sm = o2 as SpaceModel
			var n = sm.getVertex(new Tuple(0,0))
			assertNotNull( n )
			assertEquals(2,n.poset.size())
			assertTrue(n.poset.contains(sm.getVertex(new Tuple(0,1))))
			assertTrue(n.poset.contains(sm.getVertex(new Tuple(1,0))))
			assertTrue(n.preset.contains(sm.getVertex(new Tuple(0,1))))
			assertTrue(n.preset.contains(sm.getVertex(new Tuple(1,0))))
			assertTrue(n.getValuesTo(sm.getVertex(new Tuple(0,1)),"w",typeof(Integer)).size()>0)
			assertTrue(n.isInArea("border"));
			
		]
	}
	
	
}