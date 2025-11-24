       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCULAR-SHIFT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77  W-X-STRING  PIC X(100).
       77  W-X-LENGTH  PIC S9(10) COMP.
       77  W-EFFECTIVE-SHIFT PIC S9(10).
       77  W-I          PIC S9(10).
       77  W-TMP        PIC X.
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-SHIFT PIC S9(10).
           05 RESULT PIC X(100).

      * Circular shift the digits of the integer x, shift the digits right by shift
      * and return the result as a string.
      * If shift > number of digits, return digits reversed.
      * >>> circular_shift(12, 1)
      * "21"
      * >>> circular_shift(12, 2)
      * "12"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       * Convert X to string
           MOVE FUNCTION NUMVAL-C(L-X) TO W-X-STRING
           UNSTRING W-X-STRING
               DELIMITED BY SPACE
               INTO W-X-STRING
               WITH POINTER W-X-LENGTH.

       * Calculate effective shift
           COMPUTE W-EFFECTIVE-SHIFT = L-SHIFT MOD W-X-LENGTH.

       * Perform circular shift
           IF W-EFFECTIVE-SHIFT > 0
               PERFORM MOVE-CHARS-RIGHT
           ELSE
               PERFORM REVERSE-STRING
           END-IF.

       * Prepare RESULT
           STRING W-X-STRING(1:W-X-LENGTH) DELIMITED BY SIZE
               INTO RESULT
               WITH POINTER 1.

       * Print result for testing purpose
           DISPLAY "Result: " RESULT

           GOBACK.

       MOVE-CHARS-RIGHT.
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-EFFECTIVE-SHIFT
               MOVE W-X-STRING(W-X-LENGTH) TO W-TMP
               STRING W-X-STRING(1:W-X-LENGTH - 1) DELIMITED BY SIZE
                      W-TMP DELIMITED BY SIZE
                  INTO W-X-STRING
           END-PERFORM.

       REVERSE-STRING.
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-X-LENGTH / 2
               MOVE W-X-STRING(W-I) TO W-TMP
               COMPUTE W-J = W-X-LENGTH - W-I + 1
               MOVE W-X-STRING(W-J) TO W-X-STRING(W-I)
               MOVE W-TMP TO W-X-STRING(W-J)
           END-PERFORM.

       END PROGRAM CIRCULAR-SHIFT.
