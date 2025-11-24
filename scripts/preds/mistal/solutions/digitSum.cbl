       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGITSUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-LEN PIC 9(3) VALUE 0.
       01 WS-CHAR PIC X(1) VALUE SPACES.
       01 WS-ASCII-VALUE PIC 9(3) VALUE 0.

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
           MOVE 0 TO RESULT
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > FUNCTION LENGTH(L-S)
               MOVE L-S(WS-I:1) TO WS-CHAR
               IF WS-CHAR IS GREATER THAN OR EQUAL TO 'A'
                   AND WS-CHAR IS LESS THAN OR EQUAL TO 'Z'
                   COMPUTE WS-ASCII-VALUE = FUNCTION ORD(WS-CHAR)
                   ADD WS-ASCII-VALUE TO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM DIGITSUM.
