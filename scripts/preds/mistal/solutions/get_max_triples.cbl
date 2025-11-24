       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-MAX-TRIPLES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01  I               PIC S9(10).
       01  J               PIC S9(10).
       01  K               PIC S9(10).
       01  A-I            PIC S9(10).
       01  A-J            PIC S9(10).
       01  A-K            PIC S9(10).
       01  SUM-TRIPLE      PIC S9(10).
       01  COUNT-TRIPLES   PIC S9(10) VALUE 0.
       01  A-ARRAY.
           05 A-ELEMENT    PIC S9(10) OCCURS 1 TO 1000 TIMES DEPENDING ON L-N.

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

       MAIN-PARA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L-N
               COMPUTE A-ELEMENT(I) = (I * I) - I + 1
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L-N - 2
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > L-N - 1
                   PERFORM VARYING K FROM J + 1 BY 1 UNTIL K > L-N
                       COMPUTE SUM-TRIPLE = A-ELEMENT(I) + A-ELEMENT(J) + A-ELEMENT(K)
                       IF FUNCTION MOD(SUM-TRIPLE, 3) = 0
                           ADD 1 TO COUNT-TRIPLES
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           MOVE COUNT-TRIPLES TO RESULT
           GOBACK.
       END PROGRAM GET-MAX-TRIPLES.
