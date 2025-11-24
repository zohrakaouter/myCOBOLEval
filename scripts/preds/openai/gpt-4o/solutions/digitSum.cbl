       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGITSUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I              PIC 9(3) VALUE 1.
       01 TEMP-CHAR      PIC X.
       01 TEMP-VALUE     PIC S9(5).
       01 STRING-LENGTH  PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC S9(10).

      * Task
      * Write a function that takes a string as input and returns the sum of the upper characters only'
      * ASCII codes.
      * 
      * Examples:
      * digitSum("") => 0
      * digitSum("abAB") => 131
      * digitSum("abcCd") => 67
      * digitSum("helloE") => 69
      * digitSum("woArBld") => 131
      * digitSum("aAaaaXa") => 153
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCESS.
           MOVE ZERO TO RESULT
           INSPECT L-S TALLYING STRING-LENGTH FOR CHARACTERS

           PERFORM VARYING I FROM 1 BY 1 UNTIL I GREATER THAN STRING-LENGTH
               MOVE L-S(I:1) TO TEMP-CHAR
               IF TEMP-CHAR GREATER THAN OR EQUAL TO 'A' AND TEMP-CHAR LESS THAN OR EQUAL TO 'Z'
                   COMPUTE TEMP-VALUE = FUNCTION ORDINAL(TEMP-CHAR)
                   ADD TEMP-VALUE TO RESULT
               END-IF
           END-PERFORM

           GOBACK
       .

       END PROGRAM DIGITSUM.
