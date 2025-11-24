       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKJKASDKD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 PRIME-CANDIDATE PIC S9(10) VALUE 0.
       77 IS-PRIME       PIC X(01) VALUE "N".
       77 MAX-PRIME      PIC S9(10) VALUE -1.
       77 TEMP-RESULT    PIC S9(10) VALUE 0.
       77 I              PIC 9(02) VALUE 1.
       77 J              PIC 9(02).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * You are given a list of integers.
      * You need to find the largest prime value and return the sum of its digits.
      * 
      * Examples:
      * For lst = [0,81,12,3,1,21] the output should be 3
      * For lst = [0,8,1,2,1,7] the output should be 7
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               SET NI TO I
               IF L-LST(NI) > 1 THEN
                   MOVE L-LST(NI) TO PRIME-CANDIDATE
                   MOVE "Y" TO IS-PRIME
                   
                   PERFORM VARYING J FROM 2 BY 1 UNTIL J * J > PRIME-CANDIDATE OR IS-PRIME = "N"
                       IF REMAINDER (PRIME-CANDIDATE / J) = 0 THEN
                           MOVE "N" TO IS-PRIME
                       END-IF
                   END-PERFORM
                   
                   IF IS-PRIME = "Y" AND PRIME-CANDIDATE > MAX-PRIME THEN
                       MOVE PRIME-CANDIDATE TO MAX-PRIME
                   END-IF
               END-IF
           END-PERFORM

           IF MAX-PRIME > 0 THEN
               MOVE MAX-PRIME TO TEMP-RESULT
               
               PERFORM UNTIL TEMP-RESULT = 0
                   ADD REMAINDER(TEMP-RESULT / 10) TO RESULT
                   DIVIDE TEMP-RESULT BY 10 GIVING TEMP-RESULT
               END-PERFORM
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.

       END PROGRAM SKJKASDKD.
