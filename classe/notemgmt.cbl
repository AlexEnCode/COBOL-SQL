      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. notemgmt.
       AUTHOR. FLOETLEX.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT
               ASSIGN TO 'outputs.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.

      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 200 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 200 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(2000).

      ******************************************************************

       WORKING-STORAGE SECTION.

       01 BULLETIN-CONTENT. 
        02 FILLER             PIC X(3)  VALUE ' | ' .
        02 FS-NAME            PIC X(20) VALUE SPACE.
        02 FILLER             PIC X(3)  VALUE ' | ' . 
        02 FS-MOYENNE         PIC x(05) VALUE '99,99'.
        02 FILLER             PIC X(3)  VALUE ' | ' .
        02 FS-MAT-LIST 
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-MAT.
          05                  PIC X(5)  VALUE SPACE.
          05  FS-NOTE         PIC 99,99 VALUE 1.
          05                  PIC X(2)  VALUE ' |' .  

       01  F-INPUT-STATUS          PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS          PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           03 STUDENT-LGTH     PIC 9(03) VALUE 1.
           03 STUDENT  
               OCCURS 1 TO 999 TIMES
               DEPENDING ON STUDENT-LGTH
               INDEXED BY IDX-STUDENT.
                   05 S-LASTNAME   PIC X(10).
                   05 S-FIRSTNAME  PIC X(10).
                   05 S-AGE        PIC 9(02).

       01  DATA-COURSE.
           03 COURSE-LGTH     PIC 9(03) VALUE 1.
           03 COURSE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-COURSE. 
                   05 C-COEF       PIC 9V9.
                   05 C-LABEL      PIC X(25).
                   05 C-MOYENNE    PIC 99,99.

       01  DATA-GRADE.
           03 GRADE-LGTH      PIC 9(03) VALUE 1.
           03 GRADE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON GRADE-LGTH
               INDEXED BY IDX-GRADE. 
                   05 G-S-FULLNAME     PIC  X(40).         
                   05 G-C-LABEL        PIC  X(24).         
                   05 G-GRADE          PIC  99V99. 

       01  DATA-MOYENNE.
           03 MOYENNE-LGTH   PIC 9(03) VALUE 1.
           03 MOYENNE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON MOYENNE-LGTH
               INDEXED BY IDX-MOYENNE. 
                   05 M-S-FULLNAME PIC X(40).
                   05 M-MOYENNE    PIC 99V99 VALUE 00.

       01 WS-BULLETIN-HEAD. 
        02 FILLER               PIC X(3)     VALUE ' | ' .
        02 FILLER               PIC X(20)   
        VALUE "    NOM ELEVE       ".
        02 FILLER               PIC X(3)     VALUE ' | ' . 
        02 FILLER               PIC X(05)    VALUE " MOY ".
        02 FILLER               PIC X(3)     VALUE ' | ' . 
        02 WS-MAT-TETE. 
           04 WS-MAT-LIST 
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-MAT-LIST. 
            05 WS-MAT-N         PIC X(7)  VALUE SPACE.
            05                  PIC X(1)  VALUE '/' .
            05 WS-MAT-C         PIC 9,9   VALUE ZERO.
            05                  PIC X(1)  VALUE '|' .

       01  WS-BUFFER   PIC X(03) VALUE SPACE.
           88  WS-VALUE-NOT-PRESENT VALUE 'Y'.

       01  WS-PNT.
           03 WS-PNT-NBR      PIC Z9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.
       
       01  WS-ID             PIC 99.
       01  WS-ID2            PIC 99.
       01  WS-ID-3           PIC 99.

       01  SOMME-STUDENT-NOTE PIC 999v99.
       01  CALC-STUDENT-NOTE  PIC 999v99.
       01  COEF-FLOT          PIC 9v9.



       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE 'cblsql'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.

       01  WS-SQL-STUDENT.
           05  SQL-S-ID                 PIC 9(05).
           05  SQL-S-LASTNAME           PIC X(07).
           05  SQL-S-FIRSTNAME          PIC X(06).
           05  SQL-S-AGE                PIC 9(02).

       01  WS-SQL-COURSE.
           05  SQL-C-ID                 PIC 9(05).
           05  SQL-C-LABEL              PIC X(21).
           05  SQL-C-COEF               PIC X(03).
           05  SQL-C-COURSE-MOY         PIC 9(05).

       01  WS-SQL-GRADE.
           05 SQL-G-ID                  PIC 9(05).
           05 SQL-G-GRADE               PIC 9(05).

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

      ******************************************************************
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           
           EXEC SQL
           CREATE TABLE WS-SQL-STUDENT (
             SQL-S-ID                 INT PRIMARY KEY,
             SQL-S-LASTNAME           VAR,
             SQL-S-FIRSTNAME          VAR,
             SQL-S-AGE                INT
           );
           END-EXEC.

           EXEC SQL
           CREATE TABLE WS-SQL-COURSE (
              SQL-C-ID           INT PRIMARY KEY,
              SQL-C-LABEL        VAR,
              SQL-C-COEF         INT,
              SQL-C-COURSE-MOY   INT
           );
           END-EXEC.

           EXEC SQL
           CREATE TABLE WS-SQL-GRADE (
               SQL-G-ID INT PRIMARY KEY,
               SQL-G-GRADE INT,
               FOREIGN KEY (SQ-G-STUDENT-ID) 
               REFERENCES WS-SQL-STUDENT(SQL-S-ID),
               FOREIGN KEY (WS-G-COURSE-ID) 
               REFERENCES WS-SQL-COURSE(SQL-C-ID)
           );
           END-EXEC.

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
               
           PERFORM 0000-MAIN-START   THRU 0000-MAIN-END. 

      ******************************************************************
       0000-MAIN-START.
           PERFORM 7000-READ-START   THRU 7000-READ-END. 


           PERFORM 7100-WRITE-START  THRU 7100-WRITE-END.
       0000-MAIN-END.

           STOP RUN.

      ****************************************************************** 
       7000-READ-START.

           SET GRADE-LGTH COURSE-LGTH STUDENT-LGTH TO 1.
           OPEN INPUT F-INPUT.          

           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ERROR INPUT FILE'
               GO TO 7000-READ-END
           END-IF.

           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               IF F-INPUT-STATUS-EOF
                   GO TO 7000-READ-END
               END-IF
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 8010-HANDLE-STUDENT-START 
                           THRU 8010-HANDLE-STUDENT-END
                   WHEN '02'
                       PERFORM 8020-HANDLE-COURSE-START 
                           THRU 8020-HANDLE-COURSE-END
                       PERFORM 8030-HANDLE-GRADE-START
                           THRU 8030-HANDLE-GRADE-END
           END-PERFORM.

       7000-READ-END.
           SET GRADE-LGTH COURSE-LGTH STUDENT-LGTH DOWN BY 1.
           CLOSE F-INPUT.  
           EXIT.

      ******************************************************************
       7100-WRITE-START.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 9010-HEADER-START       THRU 9010-HEADER-END.
  
           PERFORM 9015-TABLE-HEADER-START THRU 9015-TABLE-HEADER-END.
  
           PERFORM 9030-BODY-START         THRU 9030-BODY-END.
           
           PERFORM 9019-MOYENNE-START      THRU 9019-MOYENNE-END.

           PERFORM 9020-FOOTER-START       THRU 9020-FOOTER-END.
           

       7100-WRITE-END.
           CLOSE F-OUTPUT.
           EXIT.

      ******************************************************************   
       7209-GRADE-MANAGEMENT-START.

           SET WS-ID TO 0.
           SET WS-ID2 TO 0.

           PERFORM  STUDENT-LGTH  TIMES
           ADD 1 TO WS-ID  
           INITIALIZE BULLETIN-CONTENT       
           INITIALIZE REC-F-OUTPUT 
           SET COEF-FLOT          TO 0
           SET CALC-STUDENT-NOTE  TO 0

           STRING S-FIRSTNAME(WS-ID)
           SPACE  S-LASTNAME(WS-ID) 
           DELIMITED BY SIZE
           INTO FS-NAME
                 
           PERFORM 7221-START-NOTE-MGMT 
           THRU    7221-END-NOTE-MGMT

              DIVIDE CALC-STUDENT-NOTE BY COEF-FLOT 
              GIVING CALC-STUDENT-NOTE ROUNDED
            MOVE CALC-STUDENT-NOTE      TO WS-PNT-GRADE  
            MOVE  WS-PNT-GRADE          TO FS-MOYENNE


           MOVE BULLETIN-CONTENT       TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT
 
           END-PERFORM.
           
       7209-GRADE-MANAGEMENT-END.

      ******************************************************************
       7221-START-NOTE-MGMT.

           INITIALIZE FS-MOYENNE.
           SET WS-ID-3 TO ZERO.
            PERFORM COURSE-LGTH TIMES
                  SET SOMME-STUDENT-NOTE TO 0
                  ADD  1 TO WS-ID2   
                  ADD  1 TO WS-ID-3      
           MOVE G-GRADE(WS-ID2)        TO FS-NOTE(WS-ID-3)

           MOVE G-GRADE(WS-ID2)        TO SOMME-STUDENT-NOTE
           MULTIPLY SOMME-STUDENT-NOTE BY C-COEF(WS-ID-3) 
           GIVING   SOMME-STUDENT-NOTE ROUNDED

           COMPUTE COEF-FLOT
           = C-COEF(WS-ID-3) + COEF-FLOT
           COMPUTE  CALC-STUDENT-NOTE
           =  CALC-STUDENT-NOTE + SOMME-STUDENT-NOTE
           END-PERFORM .
       
       7221-END-NOTE-MGMT.
           EXIT.
      ******************************************************************

       8010-HANDLE-STUDENT-START.

       
           MOVE R-S-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGTH).
           MOVE R-S-LASTNAME   TO S-LASTNAME(STUDENT-LGTH).
           MOVE R-S-AGE        TO S-AGE(STUDENT-LGTH).

           SET STUDENT-LGTH UP BY 1.
          
       8010-HANDLE-STUDENT-END.

      ****************************************************************** 
       8020-HANDLE-COURSE-START.

           INITIALIZE WS-BUFFER.
           SET IDX-COURSE TO 1.

           SEARCH COURSE VARYING IDX-COURSE
               AT END
                   SET WS-VALUE-NOT-PRESENT TO TRUE
               WHEN C-LABEL(IDX-COURSE) = R-C-LABEL
                   GO TO 8020-HANDLE-COURSE-END 
           END-SEARCH.

           IF WS-VALUE-NOT-PRESENT
               MOVE R-C-COEF   TO C-COEF(COURSE-LGTH)
               MOVE R-C-LABEL  TO C-LABEL(COURSE-LGTH)
               DISPLAY R-C-LABEL R-C-COEF
               DISPLAY C-LABEL(COURSE-LGTH)
           SET COURSE-LGTH UP BY 1
           END-IF.
       8020-HANDLE-COURSE-END.

      ****************************************************************** 
       8030-HANDLE-GRADE-START.
                 
           STRING S-FIRSTNAME(STUDENT-LGTH)
           SPACE S-LASTNAME(STUDENT-LGTH) 
           DELIMITED BY SIZE 
           INTO G-S-FULLNAME(GRADE-LGTH).
            
           MOVE R-C-LABEL TO G-C-LABEL(GRADE-LGTH).
           MOVE R-C-GRADE TO G-GRADE(GRADE-LGTH).  
           SET GRADE-LGTH UP BY 1.

       8030-HANDLE-GRADE-END.

      ****************************************************************** 
       9010-HEADER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:115).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(115:1).
           MOVE 'BULLETIN DE NOTES' TO REC-F-OUTPUT(33:20).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:115).
           WRITE REC-F-OUTPUT. 



       9010-HEADER-END.

      ******************************************************************
       9015-TABLE-HEADER-START.
           
           INITIALIZE REC-F-OUTPUT.
           SET WS-ID TO 0.
           PERFORM UNTIL WS-ID EQUAL COURSE-LGTH           
           ADD 1 TO WS-ID
           MOVE C-COEF(WS-ID)  TO WS-MAT-C(WS-ID)
           MOVE  C-LABEL(WS-ID) TO WS-MAT-N(WS-ID)     
           END-PERFORM.

           MOVE WS-BULLETIN-HEAD TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT
           INITIALIZE REC-F-OUTPUT.


       9015-TABLE-HEADER-END.
           EXIT.

      ******************************************************************
       9030-BODY-START.
      
           PERFORM 7209-GRADE-MANAGEMENT-START 
           THRU    7209-GRADE-MANAGEMENT-END. 

           INITIALIZE REC-F-OUTPUT.
           MOVE BULLETIN-CONTENT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
                     
       9030-BODY-END.

      ******************************************************************
       9019-MOYENNE-START.
           
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:115).
           WRITE REC-F-OUTPUT.
      
           SET WS-ID  TO 1
           SET WS-ID2 TO 1
           SET WS-ID-3 TO 1

           PERFORM COURSE-LGTH TIMES
           SET CALC-STUDENT-NOTE TO 0 

             PERFORM COURSE-LGTH TIMES
             DISPLAY G-GRADE(WS-ID) 
             ADD G-GRADE(WS-ID)    TO CALC-STUDENT-NOTE 
             DISPLAY  CALC-STUDENT-NOTE
             ADD COURSE-LGTH TO WS-ID
             END-PERFORM

           DIVIDE CALC-STUDENT-NOTE BY COURSE-LGTH 
           GIVING CALC-STUDENT-NOTE ROUNDED
           DISPLAY  CALC-STUDENT-NOTE
           MOVE CALC-STUDENT-NOTE TO C-MOYENNE(WS-ID2)

           MOVE COURSE(WS-ID2) TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT
           ADD 1 TO WS-ID-3
           ADD 1 to WS-ID2
           SET WS-ID TO WS-ID-3
           END-PERFORM. 

       9019-MOYENNE-END.
           EXIT.
      ******************************************************************
       9020-FOOTER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:115).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(115:1).
           MOVE 'NOMBRE DE' TO REC-F-OUTPUT(33:9).

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'ELEVES'     TO REC-F-OUTPUT(43:9).
           MOVE STUDENT-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'NOTES'    TO REC-F-OUTPUT(43:9).
           MOVE GRADE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'COURS'     TO REC-F-OUTPUT(43:9).
           MOVE COURSE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:115).
           WRITE REC-F-OUTPUT.

       9020-FOOTER-END.

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



