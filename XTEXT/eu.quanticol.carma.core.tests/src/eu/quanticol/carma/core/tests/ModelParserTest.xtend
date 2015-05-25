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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProvider))
class ModelParserTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	
	@Test
	def void test_ActionStub_Parse(){
	'''
	component Producer(enum a, Z){
	    store{
	        enum product := a;
	        record position := {x := 0, y := 1};
	    }
	
	    behaviour{
	        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
	        Send = [my.product > 0] send<1>{product := product - 1}.Send;
	    }
	
	    init{
	        Z;
	    }
	}
	
	component Consumer(enum a, Z){
	    
	    store{
	        enum product := a;
	    }
	
	    behaviour{
	        Consume = [my.product > 0] consume*{product := product - 1}.Consume + [my.product > 2] consumeDouble*{product := product - 2}.Consume;
	        Receive = [my.product > 0] send(z){product := product - z}.Receive;
	    }
	
	    init{
	        Z;
	    }
	}
	
	
	system Simple{
	
	    collective{
	        new Producer(1..6,Produce|Send);
	        new Consumer(1..6,Consume|Receive);
	    }
	
	    environment{
	    	
	        rate{
	        [True] produce* := 1;
	        [True] send := 1;
	        [True] produceDouble* := 1;
	        }
	    }
	}
	'''.parse.assertNoErrors
	}

	
}