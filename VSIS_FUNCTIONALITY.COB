       IDENTIFICATION DIVISION.
       PROGRAM-ID. practrack.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "C:\Users\zcint\STUDENT.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "C:\Users\zcint\REPORT.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "C:\Users\zcint\TEMP.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUDENT-ID        PIC X(10).
           05 STUDENT-NAME      PIC X(30).
           05 STUDENT-SECTION   PIC X(15).
           05 VIOLATION-DATE    PIC X(10).
           05 VIOLATION-TYPE    PIC X(20).
           05 VIOLATION-REMARKS PIC X(50).

       FD REPORT-FILE.
       01 REPORT-RECORD        PIC X(150).

       FD TEMP-FILE.
       01 TEMP-RECORD          PIC X(150).

       WORKING-STORAGE SECTION.
       01 WS-CHOICE             PIC 9 VALUE 0.
       01 WS-EOF                PIC 9 VALUE 0.
       01 WS-RECORD-FOUND                PIC 9 VALUE 0.
       01 WS-TEMP-RECORD        PIC X(150).
       01 WS-STUDENT-ID         PIC X(10).
       01 WS-STUDENT-NAME       PIC X(30).
       01 WS-STUDENT-SECTION    PIC X(15).
       01 WS-VIOLATION-DATE     PIC X(10).
       01 WS-VIOLATION-TYPE     PIC X(20).
       01 WS-VIOLATION-REMARKS  PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-CHOICE = 5
               PERFORM DISPLAY-MENU
           END-PERFORM.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY " "
           DISPLAY "- STUDENT VIOLATION TRACKING SYSTEM -"
           DISPLAY "1 - Add Student Violation"
           DISPLAY "2 - View Violations"
           DISPLAY "3 - Update Violation"
           DISPLAY "4 - Delete Violation"
           DISPLAY "5 - Exit"
           DISPLAY " "
           DISPLAY "Please enter your choice: " WITH NO ADVANCING
           ACCEPT WS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM ADD-VIOLATION
               WHEN 2
                   PERFORM VIEW-VIOLATIONS
               WHEN 3
                   PERFORM UPDATE-VIOLATION
               WHEN 4
                   PERFORM DELETE-VIOLATION
               WHEN 5
                   DISPLAY "Exiting the system..."
               WHEN OTHER
                   DISPLAY "Invalid choice! Please try again."
           END-EVALUATE.

       ADD-VIOLATION.
           OPEN EXTEND STUDENT-FILE.
           DISPLAY "- ADD VIOLATION -"
           DISPLAY "Enter Student ID: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-ID.
           DISPLAY "Enter Student Name: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-NAME.
           DISPLAY "Enter Student Section: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-SECTION.
           DISPLAY "Enter Date (YYYY-MM-DD): " WITH NO ADVANCING.
           ACCEPT WS-VIOLATION-DATE.
           DISPLAY "Enter Violation Type: " WITH NO ADVANCING.
           ACCEPT WS-VIOLATION-TYPE.
           DISPLAY "Enter Violation Remarks: " WITH NO ADVANCING.
           ACCEPT WS-VIOLATION-REMARKS.

           MOVE WS-STUDENT-ID TO STUDENT-ID.
           MOVE WS-STUDENT-NAME TO STUDENT-NAME.
           MOVE WS-STUDENT-SECTION TO STUDENT-SECTION.
           MOVE WS-VIOLATION-DATE TO VIOLATION-DATE.
           MOVE WS-VIOLATION-TYPE TO VIOLATION-TYPE.
           MOVE WS-VIOLATION-REMARKS TO VIOLATION-REMARKS.
           WRITE STUDENT-RECORD.
           DISPLAY "Violation added successfully!".
           CLOSE STUDENT-FILE.

       VIEW-VIOLATIONS.
           OPEN INPUT STUDENT-FILE.
           OPEN OUTPUT REPORT-FILE.
           DISPLAY " "
           DISPLAY "- VIEW VIOLATIONS -".
           MOVE 0 TO WS-EOF.
           PERFORM UNTIL WS-EOF = 1
               READ STUDENT-FILE INTO STUDENT-RECORD
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       STRING STUDENT-ID DELIMITED BY SIZE
                              STUDENT-NAME DELIMITED BY SIZE
                              STUDENT-SECTION DELIMITED BY SIZE
                              VIOLATION-DATE DELIMITED BY SIZE
                              VIOLATION-TYPE DELIMITED BY SIZE
                              VIOLATION-REMARKS DELIMITED BY SIZE
                              INTO WS-TEMP-RECORD
                       END-STRING
                       WRITE REPORT-RECORD FROM WS-TEMP-RECORD
                       DISPLAY "----------------------------------"
                       DISPLAY "Student ID: " STUDENT-ID
                       DISPLAY "Name: " STUDENT-NAME
                       DISPLAY "Section: " STUDENT-SECTION
                       DISPLAY "Date: " VIOLATION-DATE
                       DISPLAY "Type: " VIOLATION-TYPE
                       DISPLAY "Remarks: " VIOLATION-REMARKS
           END-PERFORM.
           DISPLAY "Violations saved to REPORT.DAT".
           CLOSE STUDENT-FILE.
           CLOSE REPORT-FILE.

       UPDATE-VIOLATION.
           OPEN INPUT STUDENT-FILE.
           OPEN OUTPUT TEMP-FILE.
           DISPLAY "- UPDATE VIOLATION -".
           DISPLAY "Enter Student ID to Update: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-ID.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-RECORD-FOUND.

           PERFORM UNTIL WS-EOF = 1
               READ STUDENT-FILE INTO STUDENT-RECORD
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF STUDENT-ID = WS-STUDENT-ID
                 DISPLAY "Enter New Student Name: " WITH NO ADVANCING
                           ACCEPT WS-STUDENT-NAME
                     DISPLAY "Enter New Section: " WITH NO ADVANCING
                           ACCEPT WS-STUDENT-SECTION
            DISPLAY "Enter New Date (YYYY-MM-DD): " WITH NO ADVANCING
                           ACCEPT WS-VIOLATION-DATE
                  DISPLAY "Enter New Violation Type: " WITH NO ADVANCING
                           ACCEPT WS-VIOLATION-TYPE
                       DISPLAY "Enter New Remarks: " WITH NO ADVANCING
                           ACCEPT WS-VIOLATION-REMARKS

                           MOVE WS-STUDENT-NAME TO STUDENT-NAME
                           MOVE WS-STUDENT-SECTION TO STUDENT-SECTION
                           MOVE WS-VIOLATION-DATE TO VIOLATION-DATE
                           MOVE WS-VIOLATION-TYPE TO VIOLATION-TYPE
                          MOVE WS-VIOLATION-REMARKS TO VIOLATION-REMARKS
                           MOVE 1 TO WS-RECORD-FOUND

                           DISPLAY "Record updated successfully!"
                       END-IF

                       WRITE TEMP-RECORD FROM STUDENT-RECORD
               END-PERFORM.

           CLOSE STUDENT-FILE.
           CLOSE TEMP-FILE.

           IF WS-RECORD-FOUND = 0
               DISPLAY "No record found for Student ID: " WS-STUDENT-ID
           ELSE
               DISPLAY "Replacing original file with updated records."
               PERFORM REPLACE-ORIGINAL-FILE
           END-IF.

           DISPLAY "Update operation complete.".

       REPLACE-ORIGINAL-FILEE.
           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT STUDENT-FILE.
           MOVE 0 TO WS-EOF.

           PERFORM UNTIL WS-EOF = 1
               READ TEMP-FILE INTO TEMP-RECORD
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       WRITE STUDENT-RECORD FROM TEMP-RECORD
           END-PERFORM.

           CLOSE TEMP-FILE.
           CLOSE STUDENT-FILE.
           DISPLAY "Original file replaced successfully.".


           IF WS-RECORD-FOUND = 0
               DISPLAY "No record found for Student ID: " WS-STUDENT-ID

           CLOSE STUDENT-FILE.


       DELETE-VIOLATION.
           OPEN INPUT STUDENT-FILE
           OPEN OUTPUT TEMP-FILE
           DISPLAY "- DELETE VIOLATION -"
           DISPLAY "Enter Student ID to Delete: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-ID
           MOVE 0 TO WS-EOF
           MOVE 0 TO WS-RECORD-FOUND
           PERFORM UNTIL WS-EOF = 1
               READ STUDENT-FILE INTO STUDENT-RECORD
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF STUDENT-ID NOT = WS-STUDENT-ID
                           WRITE TEMP-RECORD FROM STUDENT-RECORD
                       ELSE
            DISPLAY "Record with Student ID " WS-STUDENT-ID " deleted."
                           MOVE 1 TO WS-RECORD-FOUND
                       END-IF
           END-PERFORM
           CLOSE STUDENT-FILE
           CLOSE TEMP-FILE

           IF WS-RECORD-FOUND = 0
           DISPLAY "Record with Student ID " WS-STUDENT-ID " not found."
           ELSE
               DISPLAY "Replacing original file with updated records."
               PERFORM REPLACE-ORIGINAL-FILE
           END-IF

           DISPLAY "Delete operation complete.".

       REPLACE-ORIGINAL-FILE.
           OPEN INPUT TEMP-FILE
           OPEN OUTPUT STUDENT-FILE
           MOVE 0 TO WS-EOF
           PERFORM UNTIL WS-EOF = 1
               READ TEMP-FILE INTO TEMP-RECORD
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       WRITE STUDENT-RECORD FROM TEMP-RECORD
           END-PERFORM
           CLOSE TEMP-FILE
           CLOSE STUDENT-FILE
           DISPLAY "Original file replaced successfully.".
