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
        Shout = listen<>{shout := shout + 1}.Shout;
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
			default : 0.0;
        }

        rate{
			default : 1.0;
        }

        update{

        }
    }
}
	'''.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
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
        Shout = listen<>{shout := shout + 1}.Shout;
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
			default : 0.0;
        }

        rate{
			default : 1.0;
        }

        update{

        }
    }
}
'''.compile[ getCompiledClass.newInstance ]
	}
	
}