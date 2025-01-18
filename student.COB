      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 VIOLATION       PIC S9(2) VALUE 99.
           01 CLRSCR          PIC X(3) VALUE 'CLS'.
           01 USERNAME        PIC X(50).
           01 PASSWORD        PIC X(50).
           01 ATTEMPT         PIC 9(1) VALUE 3.
           01 ACCURATE        PIC X VALUE 'B'.
           01 STUDENT-NUM     PIC X(50).
           01 FULL-NAME       PIC X(50).
           01 COURSE          PIC X(50).
           01 YEAR-SECTION    PIC X(50).
           01 DATE1           PIC X(50).

       PROCEDURE DIVISION.
           CALL "SYSTEM" USING CLRSCR.
           PERFORM AUTHENTICATION-SECTION UNTIL ACCURATE = "A"
           PERFORM STUDENT-INFO
           STOP RUN.

       AUTHENTICATION-SECTION.
           CALL "SYSTEM" USING 'CLS'.
           DISPLAY "STUDENT VIOLATION TRACKING SYSTEM"
           DISPLAY " "
           DISPLAY "=================================="
           DISPLAY "        LOGIN YOUR ACCOUNT "
           DISPLAY "=================================="
           DISPLAY " "
           DISPLAY "             ADMIN "
           DISPLAY " "
           DISPLAY "Username: "
           ACCEPT USERNAME.
           DISPLAY "Password: "
           ACCEPT PASSWORD.

           IF PASSWORD = USERNAME
               DISPLAY " "
               DISPLAY " "
               DISPLAY "ACCESS GRANTED!"
               MOVE "A" TO ACCURATE
           ELSE
               DISPLAY " "
               DISPLAY " "
               DISPLAY "ACCESS DENIED! PLEASE TRY AGAIN!"
               MOVE "B" TO ACCURATE

           STOP RUN.

           CLEAR-SCREEN.
               CALL 'SYSTEM' USING CLRSCR.

           PERFORM STUDENT-INFO.

           STUDENT-INFO.
               CALL "SYSTEM" USING 'CLS'.
               DISPLAY " "
               DISPLAY "++++++++++++++++++++++++++++++++++++++++++++"
               DISPLAY "           STUDENT INFORMATIONS"
               DISPLAY "++++++++++++++++++++++++++++++++++++++++++++"
               DISPLAY "STUDENT NUMBER: "
               ACCEPT STUDENT-NUM.
               DISPLAY "FULL NAME: "
               ACCEPT FULL-NAME.
               DISPLAY "PROGRAM: "
               ACCEPT COURSE.
               DISPLAY "YEAR AND SECTION: "
               ACCEPT YEAR-SECTION.
               DISPLAY "DATE (YYYY-MM-DD): "
               ACCEPT DATE1.

               DISPLAY "IS THIS YOUR FIRST TIME? (Y/N)"
               DISPLAY "ENTER VIOLATION: "
               ACCEPT VIOLATION.
               PERFORM CLEAR-SCREEN

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
