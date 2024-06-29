      ***************************************************************** 
      *         export COB_LDFLAGS=-Wl,--no-as-needed
      *         export COBCPY=./Copybook
      *         ocesql banksql.cbl prog.cob
      *         cobc -locesql -x -o run prog.cob 
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. banksql.
       AUTHOR. AlexEnCde.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ****************************************************************** 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Le fichier de sortie
           SELECT RAPPORT ASSIGN TO 'rapport.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS.
           
      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.

       FD RAPPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V.    

       01  RAPPORT-ENTRY     PIC X(125).
      ******************************************************************

       WORKING-STORAGE SECTION.

      *****************************************
      *           VARIABLE STATUS             *
      *****************************************      
       01 REC-ASSU-STATUS        PIC X(2).

      *****************************************
      *        VARIABLE RAPPORT               *
      ***************************************** 
       01  PT-ENTETE    PIC X(27) VALUE '*   TRAITEMENT DATABANK   *'.
       01  PT-ETOILE    PIC X(27) VALUE '***************************'.
       01  PT-MINMAX    PIC X(27) VALUE '*     AGE MIN ET MAX      *'.
       01  PT-COUNTER   PIC X(27) VALUE '*      NOMBRE PAR AGE     *'.
       01  PT-BELGIUM   PIC X(27) VALUE '*       BELGOPHONE        *'.

      *****************************************
      *        VARIABLE AFFICHAGE             *
      *****************************************  
       01  MIN-ET-MAX.
       05  FILLER               PIC X(15) VALUE "L'age max est: ".
       05  WS-MAX-AGE           PIC 9(2).
       05  FILLER               PIC X(6) VALUE " ans. ".
       05  FILLER               PIC X(15) VALUE "L'age min est: ".
       05  WS-MIN-AGE           PIC 9(2).
       05  FILLER               PIC X(5) VALUE " ans.".

       01  WS-BELGIAN.
           05 WS-NAME.
            10  WS-B-FISTNAME         PIC X(10).
            10 FILLER                 PIC X(01) VALUE SPACE.
            10  WS-B-LASTNAME         PIC X(10).
           05 FILLER                  PIC X(07) VALUE 'mail : '.            
           05  WS-B-EMAIL             PIC X(30).
           05 FILLER                  PIC X(08) VALUE 'quote : '.           
           05  WS-B-QUOTE             PIC X(30). 
       
       01  AGE.
           05  WS-AGE            PIC 9(03).
           05  FILLER            PIC X(7) VALUE " ANS :".
           05  WS-AGE-COUNTER    PIC 9(03).

      *****************************************
      *              VARIABLE SQL             *
      ***************************************** 

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE 'cobol'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.

       01 PHRASE.
	       05 PH-COUNTRY-CODE   PIC X(50).
	       05 PH-PHRASE         PIC X(50).

       01  DATABANK.
           05 DK-FIRST-NAME     PIC X(50).
	       05 DK-LAST-NAME      PIC X(50).
	       05 DK-EMAIL          PIC X(50).
	       05 DK-GENDER         PIC X(50).
	       05 DK-AGE            PIC 9(10).   
           05 DK-SPOKEN         PIC X(50).
	       05 DK-COUNTRY        PIC X(50).
	       05 DK-COUNTRY-CODE   PIC X(50).
	       05 DK-INFO-PHONE     PIC X(50).    

       01  SQL-BELGIAN.
           05  SQL-B-FISTNAME         PIC X(50). 
           05  SQL-B-LASTNAME         PIC X(50).
           05  SQL-B-EMAIL            PIC X(50).
           05  SQL-B-QUOTE            PIC X(50). 

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

      ******************************************************************
           EXEC SQL
               CONNECT :USERNAME 
               IDENTIFIED BY :PASSWD 
               USING :DBNAME 
           END-EXEC.

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.


           PERFORM 0000-MAIN-START   THRU 0000-MAIN-END. 

      ******************************************************************
       0000-MAIN-START.

           OPEN OUTPUT RAPPORT.
           CLOSE RAPPORT.
           OPEN EXTEND RAPPORT.

           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ENTETE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           PERFORM 7010-AGE-START          THRU 7010-AGE-END. 
           PERFORM 7020-AGECOUNT-START     THRU 7020-AGECOUNT-END. 
           PERFORM 7030-BELGE-START        THRU 7030-BELGE-END. 

           CLOSE RAPPORT.

       0000-MAIN-END.
           STOP RUN.

      ****************************************************************** 
       7010-AGE-START.

      *****************************************
      *           Ages min et max             *
      *****************************************    

           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-MINMAX TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           EXEC SQL
           SELECT MAX(age) INTO :WS-MAX-AGE
           FROM databank
           END-EXEC.  

           
           EXEC SQL
               SELECT MIN(age) INTO :WS-MIN-AGE
               FROM databank
           END-EXEC.
             
           MOVE MIN-ET-MAX TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

       7010-AGE-END. 
           EXIT.

      ****************************************************************** 
       7020-AGECOUNT-START.           
      *****************************************
      *           Recherche par ages          *
      *****************************************    

           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-COUNTER TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           EXEC SQL 
           DECLARE CRAGE CURSOR FOR
               SELECT age, COUNT(*)
               FROM databank
               GROUP BY age
               ORDER BY age ASC
           END-EXEC.

           MOVE "lISTE DES AGES :" TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
  
            EXEC SQL
            OPEN CRAGE 
            END-EXEC.
         
           PERFORM UNTIL SQLCODE = +100
             EXEC SQL
             FETCH CRAGE
             INTO :WS-AGE, :WS-AGE-COUNTER
             END-EXEC
               IF SQLCODE = 0 THEN
               MOVE AGE TO RAPPORT-ENTRY
               WRITE RAPPORT-ENTRY
               END-IF
           END-PERFORM.

       7020-AGECOUNT-END. 
           EXIT.
           
      ****************************************************************** 
       7030-BELGE-START.      

      *****************************************
      *     Recherche par belgophoniste       *
      *****************************************    

           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-BELGIUM TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           EXEC SQL DECLARE CRBELGE CURSOR FOR
               SELECT last_name, first_name, email, phrase
               FROM databank, phrase
               WHERE country = 'Belgium'
           END-EXEC.

           EXEC SQL 
           OPEN CRBELGE
           END-EXEC.

           PERFORM WITH TEST AFTER UNTIL SQLCODE = +100
               EXEC SQL
                   FETCH CRBELGE
                   INTO :SQL-B-FISTNAME, 
                        :SQL-B-LASTNAME, 
                        :SQL-B-EMAIL,    
                        :SQL-B-QUOTE     
               END-EXEC

               IF SQLCODE = 0 THEN
                   MOVE SQL-B-FISTNAME,  TO WS-B-FISTNAME, 
                   MOVE SQL-B-LASTNAME,  TO WS-B-LASTNAME, 
                   MOVE SQL-B-EMAIL,     TO WS-B-EMAIL,    
                   MOVE SQL-B-QUOTE      TO WS-B-QUOTE     

                   MOVE WS-BELGIAN TO RAPPORT-ENTRY
                   WRITE RAPPORT-ENTRY
               END-IF
           END-PERFORM.

         
       7030-BELGE-END.
           EXIT.

      ******************************************************************
      *                     ERROR SQL MGMT                             * 
      ******************************************************************
       1001-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-ERROR-RTN-END.
           STOP RUN. 
      ****************************************************************** 
