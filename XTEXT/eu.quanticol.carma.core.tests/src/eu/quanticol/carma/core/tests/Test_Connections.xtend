package eu.quanticol.carma.core.tests

import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.InjectWith
import eu.quanticol.carma.core.CARMAInjectorProvider
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
class Test_Connections {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	@Test
	def void test_Parser(){
	'''
component Client(){

    store{
	
		attrib lost 	:= 0;
		attrib resets	:= 0;
		
		attrib sends 	:= 0;
		attrib receives := 0;

    }

    behaviour{
		Busy_C 	= 	busy*.Ack_C;
		
		Ack_C 	= 	[lost < 3]ack<>{sends := sends + 1}.Listen_C + 
					[lost < 3]lost*{lost := lost + 1}.Ack_C + 
					[lost > 2]reset*{lost := 0, resets := resets + 1}.Busy_C;
					
		Listen_C	=	timeout*.Timeout_C + syn(){receives := receives + 1}.Work_C;
		Timeout_C 	=	reset*{lost := 0, resets := resets + 1}.Busy_C;
		Work_C		=	work<>.Close_C;
		Close_C		=	close<>.Busy_C;

    }

    init{
		Busy_C
    }
}

component Server(){

    store{
		
		attrib sends 	:= 0;
		attrib receives := 0;
    }

    behaviour{
		
		Listen_S 	= ack(){receives := receives + 1}.Syn_S;
		Syn_S		= syn<>{sends := sends + 1}.Work_S;
		Work_S		= work().Close_S;
		Close_S		= close().Listen_S;

		
    }

    init{
		Listen_S
    }
}



measure ClientSends = #{ Client[*]  | my.sends > 0 };
measure ClientReceives = #{ Client[*]  | my.receives > 0 };
measure ServerSends = #{ Server[*]  | my.sends > 0 };
measure ServerReceives = #{ Server[*]  | my.receives > 0 };


system Simple{

    collective{
    	for(i ; i < 100; i + 1){
    		new Client();
    	} 
		new Server();
    }

    environment{

        store{

        }

        prob{
			default : 1.0;
        }

        rate{
        	[true] ack : 10.0;
        	[true] syn : 10.0;
        	[true] timeout : 0.5;
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
component Client(){

    store{
	
		attrib lost 	:= 0;
		attrib resets	:= 0;
		
		attrib sends 	:= 0;
		attrib receives := 0;

    }

    behaviour{
		Busy_C 	= 	busy*.Ack_C;
		
		Ack_C 	= 	[lost < 3]ack<>{sends := sends + 1}.Listen_C + 
					[lost < 3]lost*{lost := lost + 1}.Ack_C + 
					[lost > 2]reset*{lost := 0, resets := resets + 1}.Busy_C;
					
		Listen_C	=	timeout*.Timeout_C + syn(){receives := receives + 1}.Work_C;
		Timeout_C 	=	reset*{lost := 0, resets := resets + 1}.Busy_C;
		Work_C		=	work<>.Close_C;
		Close_C		=	close<>.Busy_C;

    }

    init{
		Busy_C
    }
}

component Server(){

    store{
		
		attrib sends 	:= 0;
		attrib receives := 0;
    }

    behaviour{
		
		Listen_S 	= ack(){receives := receives + 1}.Syn_S;
		Syn_S		= syn<>{sends := sends + 1}.Work_S;
		Work_S		= work().Close_S;
		Close_S		= close().Listen_S;

		
    }

    init{
		Listen_S
    }
}


measure ClientSends = #{ Client[*]  | my.sends > 0 };
measure ClientReceives = #{ Client[*]  | my.receives > 0 };
measure ServerSends = #{ Server[*]  | my.sends > 0 };
measure ServerReceives = #{ Server[*]  | my.receives > 0 };


system Simple{

    collective{
    	for(i ; i < 100; i + 1){
    		new Client();
    	} 
		new Server();
    }

    environment{

        store{

        }

        prob{
			default : 1.0;
        }

        rate{
        	[true] ack : 10.0;
        	[true] syn : 10.0;
        	[true] timeout : 0.5;
			default : 1.0;
        }

        update{

        }
    }
}

	'''.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			var m = o as CarmaModel
			assertEquals( 1 , m.systems.length )
			assertEquals( 4 , m.measures.length )					

	]
	}
	
}