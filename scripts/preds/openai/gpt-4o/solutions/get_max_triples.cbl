       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-MAX-TRIPLES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  A-ARRAY.
           05 A-ELEM OCCURS 1000 TIMES PIC S9(10).

       01  I          PIC S9(10) VALUE 0.
       01  J          PIC S9(10) VALUE 0.
       01  K          PIC S9(10) VALUE 0.
       01  N          PIC S9(10).
       01  COUNT      PIC S9(10) VALUE 0.
       01  SUM        PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * You are given a positive integer n. You have to create an integer array a of length n.
      * For each i (1 ≤ i ≤ n), the value of a[i] = i * i - i + 1.
      * Return the number of triples (a[i], a[j], a[k]) of a where i < j < k,
      * and a[i] + a[j] + a[k] is a multiple of 3.
      * 
      * Example :
      * Input: n = 5
      * Output: 1
      * Explanation:
      * a = [1, 3, 7, 13, 21]
      * The only valid triple is (1, 7, 13).
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       INITIALIZE-PROCESS.

           MOVE L-N TO N.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               COMPUTE A-ELEM(I) = (I * I) - I + 1
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - 2
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > N - 1
                   PERFORM VARYING K FROM J + 1 BY 1 UNTIL K > N
                       COMPUTE SUM = A-ELEM(I) + A-ELEM(J) + A-ELEM(K)
                       IF SUM MOD 3 = 0
                           ADD 1 TO COUNT
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           MOVE COUNT TO RESULT.

       STOP RUN.
