       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMERICAL-LETTER-GRADE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC 99 VALUE 0.
       01 TEMP-GRADE PIC 9(1)V9(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-GRADES OCCURS 5 TIMES INDEXED BY NI COMP-2.
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * It is the last week of the semester and the teacher has to give the grades
      * to students. The teacher has been making her own algorithm for grading.
      * The only problem is, she has lost the code she used for grading.
      * She has given you a list of GPAs for some students and you have to write
      * a function that can output a list of letter grades using the following table:
      * GPA       |    Letter grade
      * 4.0                A+
      * > 3.7                A
      * > 3.3                A-
      * > 3.0                B+
      * > 2.7                B
      * > 2.3                B-
      * > 2.0                C+
      * > 1.7                C
      * > 1.3                C-
      * > 1.0                D+
      * > 0.7                D
      * > 0.0                D-
      * 0.0                E
      * 
      * 
      * Example:
      * grade_equation([4.0, 3, 1.7, 2, 3.5]) ==> ['A+', 'B', 'C-', 'C', 'A-']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               SET NI TO I
               MOVE L-GRADES(NI) TO TEMP-GRADE

               EVALUATE TRUE
                   WHEN TEMP-GRADE = 4.0
                       MOVE 'A+' TO RESULT(I)
                   WHEN TEMP-GRADE > 3.7
                       MOVE 'A' TO RESULT(I)
                   WHEN TEMP-GRADE > 3.3
                       MOVE 'A-' TO RESULT(I)
                   WHEN TEMP-GRADE > 3.0
                       MOVE 'B+' TO RESULT(I)
                   WHEN TEMP-GRADE > 2.7
                       MOVE 'B' TO RESULT(I)
                   WHEN TEMP-GRADE > 2.3
                       MOVE 'B-' TO RESULT(I)
                   WHEN TEMP-GRADE > 2.0
                       MOVE 'C+' TO RESULT(I)
                   WHEN TEMP-GRADE > 1.7
                       MOVE 'C' TO RESULT(I)
                   WHEN TEMP-GRADE > 1.3
                       MOVE 'C-' TO RESULT(I)
                   WHEN TEMP-GRADE > 1.0
                       MOVE 'D+' TO RESULT(I)
                   WHEN TEMP-GRADE > 0.7
                       MOVE 'D' TO RESULT(I)
                   WHEN TEMP-GRADE > 0.0
                       MOVE 'D-' TO RESULT(I)
                   WHEN OTHER
                       MOVE 'E' TO RESULT(I)
               END-EVALUATE
           END-PERFORM.

           GOBACK.
       END PROGRAM NUMERICAL-LETTER-GRADE.
