       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIME-LENGTH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-LENGTH PIC 9(3).
           05 WS-I PIC 9(3).
           05 WS-PRIME-FLAG PIC 9 VALUE 1.
               88 IS-PRIME VALUE 1.
               88 NOT-PRIME VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC 9.

      * Write a function that takes a string and returns True if the string
      * length is a prime number or False otherwise
      * Examples
      * prime_length('Hello') == True
      * prime_length('abcdcba') == True
      * prime_length('kittens') == True
      * prime_length('orange') == False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(L-STRING)

           IF WS-LENGTH = 1
               SET NOT-PRIME TO TRUE
           ELSE
               PERFORM VARYING WS-I FROM 2 BY 1
                       UNTIL WS-I * WS-I > WS-LENGTH OR NOT IS-PRIME
                   IF FUNCTION MOD(WS-LENGTH, WS-I) = 0
                       SET NOT-PRIME TO TRUE
                   END-IF
               END-PERFORM
           END-IF

           IF IS-PRIME
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM PRIME-LENGTH.
