       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERATE-INTEGERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 W-START PIC S9(10).
       01 W-END PIC S9(10).
       01 W-TEMP PIC S9(10).
       01 I PIC S9(10).
       01 COUNT PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given two positive integers a and b, return the even digits between a
      * and b, in ascending order.
      * 
      * For example:
      * generate_integers(2, 8) => [2, 4, 6, 8]
      * generate_integers(8, 2) => [2, 4, 6, 8]
      * generate_integers(10, 14) => []
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE L-A TO W-START
           MOVE L-B TO W-END

           IF W-START > W-END
               MOVE W-START TO W-TEMP
               MOVE W-END TO W-START
               MOVE W-TEMP TO W-END
           END-IF

           PERFORM VARYING I FROM W-START BY 1 UNTIL I > W-END
               IF I MOD 2 = 0
                   ADD 1 TO COUNT
                   SET RESULT(COUNT) TO I
               END-IF
           END-PERFORM

           DISPLAY 'Even numbers between ' L-A ' and ' L-B ':'
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > COUNT
               DISPLAY RESULT(NI)
           END-PERFORM

           GOBACK.

       END PROGRAM GENERATE-INTEGERS.
