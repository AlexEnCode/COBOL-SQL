       IDENTIFICATION DIVISION.
       PROGRAM-ID. payssql.
       AUTHOR. AlexEnCode.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ****************************************************************** 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Le fichier de sortie
           SELECT RAPPORT ASSIGN TO 'rapport2.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS.
           
      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.

       FD RAPPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.    

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
       01  PT-ENTETE    PIC X(27) VALUE '* TRAITEMENT DATABANK P2  *'.
       01  PT-ETOILE    PIC X(27) VALUE '***************************'.
       01  PT-CODE      PIC X(27) VALUE '*     CORRECTION CODE     *'.
       01  PT-INCOH     PIC X(27) VALUE '* CORRECTION INCOHERENCES *'.
       01  PT-MAJ       PIC X(27) VALUE '*  CORRECTION MAJUSCULE   *'. 

      *****************************************
      *             VARIABLE ALGO             *
      ***************************************** 

       01  WS-COUNTRY        PIC X(50) VALUE SPACE.
       01  WS-CODE           PIC X(50) VALUE SPACE.
       01  WS-COUNTRY-CODE   PIC X(50) VALUE SPACE.

      *****************************************
      *              VARIABLE SQL             *
      ***************************************** 

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE 'cobol'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.

       01  DATABANK.
           05 DK-ID             PIC X(50).
           05 DK-FIRST-NAME     PIC X(50).
	       05 DK-LAST-NAME      PIC X(50).
	       05 DK-EMAIL          PIC X(50).
	       05 DK-GENDER         PIC X(50).
	       05 DK-AGE            PIC 9(10).   
           05 DK-SPOKEN         PIC X(50).
	       05 DK-COUNTRY        PIC X(50).
	       05 DK-COUNTRY-CODE   PIC X(50).
	       05 DK-INFO-PHONE     PIC X(50).    


       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

      ******************************************************************
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
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

           INITIALIZE RAPPORT-ENTRY.
           MOVE PT-ETOILE   TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-ENTETE   TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-ETOILE   TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.     

           PERFORM 7010-CODE-START          THRU 7010-CODE-END. 
           PERFORM 7020-INCOHERENCE-START   THRU 7020-INCOHERENCE-END. 
           PERFORM 7030-MAJUSCULE-START     THRU 7030-MAJUSCULE-END. 

           CLOSE RAPPORT.

           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           
       0000-MAIN-END.
           STOP RUN.

      ******************************************************************
       7010-CODE-START. 

      *****************************************
      *     MAJ CODE PAYS FR VERS BE          *
      *****************************************    
           INITIALIZE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-CODE  TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           EXEC SQL
                UPDATE databank
                SET country_code = 'BE'
                WHERE age > 35 AND age < 40
                AND country_code = 'FR'
            END-EXEC.

    
           IF SQLCODE = 0 THEN
               INITIALIZE RAPPORT-ENTRY           
               MOVE 'La mise à jour à correctement été effectuée.'
               TO RAPPORT-ENTRY
               WRITE RAPPORT-ENTRY
           END-IF.   

           DISPLAY "part1 ok".

       7010-CODE-END.


      ****************************************************************** 
       7020-INCOHERENCE-START.   

      *****************************************
      *     INCOHERENCE PAYS / CODE PAYS      *
      *****************************************    

           INITIALIZE RAPPORT-ENTRY.  
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-INCOH  TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           INITIALIZE RAPPORT-ENTRY.           
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
         
           DISPLAY "switch ok"

           EXEC SQL 
               DECLARE CRCODE CURSOR FOR
               SELECT country, 
                      country_code
               FROM databank
           END-EXEC.

           EXEC SQL 
               OPEN CRCODE
           END-EXEC.

           DISPLAY "cursor ok".
           
           PERFORM UNTIL SQLCODE = +100
      
               EXEC SQL
                   FETCH CRCODE
                   INTO :DK-COUNTRY, :DK-COUNTRY-CODE
               END-EXEC
               
               IF SQLCODE = 0 THEN
               MOVE DK-COUNTRY          TO WS-COUNTRY
               MOVE DK-COUNTRY-CODE     TO WS-CODE         
      
               IF WS-COUNTRY = 'France'           THEN
                   MOVE 'FR' TO WS-COUNTRY-CODE
               ELSE IF WS-COUNTRY = 'Belgium'     THEN
                   MOVE 'BE' TO WS-COUNTRY-CODE
               ELSE IF WS-COUNTRY = 'Luxembourg'  THEN
                   MOVE 'LU' TO WS-COUNTRY-CODE
               ELSE IF WS-COUNTRY = 'Switzerland' THEN
                   MOVE 'CH' TO WS-COUNTRY-CODE
               ELSE
                   MOVE '  ' TO WS-COUNTRY-CODE
               END-IF    
               IF WS-CODE NOT EQUAL TO WS-COUNTRY-CODE THEN
               EXEC SQL
                   UPDATE databank
                   SET country_code = :WS-COUNTRY-CODE
                   WHERE id = :DK-ID
               END-EXEC
           
               DISPLAY "update ok"
               END-IF
               END-IF           
           END-PERFORM. 

           EXEC SQL 
               CLOSE CRCODE
           END-EXEC.

           IF SQLCODE = 0 THEN
           INITIALIZE RAPPORT-ENTRY           
               MOVE 'La mise à jour à correctement été effectuée.'
               TO RAPPORT-ENTRY
               WRITE RAPPORT-ENTRY
           ELSE
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.

           DISPLAY "part2 ok".
       7020-INCOHERENCE-END.


      ******************************************************************
       7030-MAJUSCULE-START. 
  
      *****************************************
      *   MISE EN MAJUSCULE PAYS ET LANGUE    *
      *****************************************    

           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-MAJ    TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE PT-ETOILE TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.

           EXEC SQL
               UPDATE databank
               SET country = UPPER(country),
                   spoken  = UPPER(spoken)
           END-EXEC.

           IF SQLCODE = 0 THEN
               MOVE 'La mise à jour à correctement été effectuée.'
               TO RAPPORT-ENTRY
               WRITE RAPPORT-ENTRY
           ELSE
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
           DISPLAY "part3 ok".
       7030-MAJUSCULE-END. 


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
