## Commande pour le copybook de OceSQL

          export COB_LDFLAGS=-Wl,--no-as-needed   
               export COBCPY=./Copybook                 
               ocesql payssql.cbl prog.cob            
               cobc -locesql -x -v -o  run prog.cob

          export COB_LDFLAGS=-Wl,--no-as-needed   
               export COBCPY=./Copybook                 
               ocesql banksql.cbl prog.cob            
               cobc -locesql -x -v -o  run prog.cob
