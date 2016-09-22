package eu.quanticol.carma.core.tests

import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.InjectWith
import com.google.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_ProbTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	@Test
	def void test_Parser(){
	'''
component Listen(){

    store{
        attrib listen := 0;
    }

    behaviour{
        Listen = listen(){listen := listen + 1;}.Listen;
    }

    init{
        Listen
    }
}

component Shout(){

    store{
        attrib shout := 0;
    }

    behaviour{
        Shout = listen<>{shout := shout + 1;}.Shout;
    }

    init{
        Shout
    }
}



measure Listening = #{ Listen[*]  | my.listen > 0 };
measure Shouting = #{ Shout[*]  | my.shout > 0 };



system Simple{


    collective{
		new Listen();
		new Shout();
    }

    environment{

        store{

        }

        prob{
			default { return 0.0; }
        }

        rate{
			default { return 1.0; }
        }

        update{

        }
    }
}
	'''.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		
	'''
component Listen(){

    store{
        attrib listen := 0;
    }

    behaviour{
        Listen = listen(){listen := listen + 1}.Listen;
    }

    init{
        Listen
    }
}

component Shout(){

    store{
        attrib shout := 0;
    }

    behaviour{
        Shout = listen<>{shout := shout + 1;}.Shout;
    }

    init{
        Shout
    }
}



measure Listening = #{ Listen[*]  | my.listen > 0 };
measure Shouting = #{ Shout[*]  | my.shout > 0 };



system Simple{


    collective{
		new Listen();
		new Shout();
    }

    environment{

        store{

        }

        prob{
			default { return 0.0; }
        }

        rate{
			default { return 1.0; }
        }

        update{

        }
    }
}
'''.compile[ 			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			var m = o as CarmaModel
			assertEquals( 1 , m.systems.length )
			assertEquals( 2 , m.measures.length )					
]
	}
	
}