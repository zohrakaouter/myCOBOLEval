       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNIQUE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 L-COUNT          PIC S9(10) COMP-3 VALUE 0.
       01 I                PIC S9(10) COMP-3.
       01 J                PIC S9(10) COMP-3.
       01 TEMP-VAR         PIC S9(10).
       01 LENGTH           PIC S9(10) COMP-3.
       01 UNIQUE-COUNT     PIC S9(10) COMP-3 VALUE 0.
       01 IS-UNIQUE        PIC X VALUE 'Y'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 9 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Return sorted unique elements in a list
      * >>> unique([5, 3, 5, 2, 3, 3, 9, 0, 123])
      * [0, 2, 3, 5, 9, 123]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
               SET NI TO I
               MOVE L-L(NI) TO TEMP-VAR
               MOVE 'Y' TO IS-UNIQUE
               
               PERFORM VARYING J FROM 1 BY 1 UNTIL J >= I
                   IF L-L(J) = TEMP-VAR
                       MOVE 'N' TO IS-UNIQUE
                   END-IF
               END-PERFORM

               IF IS-UNIQUE = 'Y'
                   ADD 1 TO UNIQUE-COUNT
                   SET NJ TO UNIQUE-COUNT
                   MOVE TEMP-VAR TO RESULT(NJ)
               END-IF

           END-PERFORM
           
       * Sort the unique elements

           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= UNIQUE-COUNT
               PERFORM VARYING J FROM I+1 BY 1 UNTIL J > UNIQUE-COUNT
                   IF RESULT(I) > RESULT(J)
                       MOVE RESULT(I) TO TEMP-VAR
                       MOVE RESULT(J) TO RESULT(I)
                       MOVE TEMP-VAR TO RESULT(J)
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK.
          
       END PROGRAM UNIQUE.
