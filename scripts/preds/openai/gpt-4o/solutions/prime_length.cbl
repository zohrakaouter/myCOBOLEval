       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIME-LENGTH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 STR-LENGTH     PIC 9(3) VALUE 0.
       01 I              PIC 9(3) VALUE 0.
       01 DIVISOR-COUNT  PIC 9(3) VALUE 0.
       01 IS-PRIME       PIC X VALUE 'F'.

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

           * Calculate the length of the input string L-STRING
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > 100 OR L-STRING(I:1) = SPACE
               ADD 1 TO STR-LENGTH
           END-PERFORM.

           * Check if STR-LENGTH is a prime number
           IF STR-LENGTH > 1 THEN
               SET IS-PRIME TO 'T'
               PERFORM VARYING I FROM 2 BY 1
                   UNTIL I > STR-LENGTH / 2 OR IS-PRIME = 'F'
                   IF STR-LENGTH MOD I = 0 THEN
                       SET IS-PRIME TO 'F'
                   END-IF
               END-PERFORM
           ELSE
               SET IS-PRIME TO 'F'
           END-IF.

           * Set the RESULT based on IS-PRIME
           IF IS-PRIME = 'T' THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

           GOBACK.

       END PROGRAM PRIME-LENGTH.
