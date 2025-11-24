       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAS-CLOSE-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC 99.
       01 J PIC 99.
       01 DIFFERENCE COMP-2.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI COMP-2.
           05 L-THRESHOLD COMP-2.
           05 RESULT PIC 9.

      * Check if in given list of numbers, are any two numbers closer to each other than
      * given threshold.
      * >>> has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0], 0.5)
      * False
      * >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0], 0.3)
      * True
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE 0 TO RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 5
                   COMPUTE DIFFERENCE = FUNCTION ABS(L-NUMBERS(I) - L-NUMBERS(J))
                   IF DIFFERENCE < L-THRESHOLD
                       MOVE 1 TO RESULT
                       GOBACK
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK.
       END PROGRAM HAS-CLOSE-ELEMENTS.
