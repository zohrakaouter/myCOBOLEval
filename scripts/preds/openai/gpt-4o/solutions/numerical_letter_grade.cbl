       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMERICAL-LETTER-GRADE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TABLE.
           05 WS-GRADES OCCURS 5 TIMES INDEXED BY I.
               10 WS-GPA PIC 9V9 COMP-2.
       
       01 GRADE-PARAM.
           05 GRADE PIC X(2).

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

       BEGIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE L-GRADES(I) TO WS-GPA
               EVALUATE TRUE
                   WHEN WS-GPA = 4.0
                       MOVE 'A+' TO GRADE
                   WHEN WS-GPA > 3.7
                       MOVE 'A' TO GRADE
                   WHEN WS-GPA > 3.3
                       MOVE 'A-' TO GRADE
                   WHEN WS-GPA > 3.0
                       MOVE 'B+' TO GRADE
                   WHEN WS-GPA > 2.7
                       MOVE 'B' TO GRADE
                   WHEN WS-GPA > 2.3
                       MOVE 'B-' TO GRADE
                   WHEN WS-GPA > 2.0
                       MOVE 'C+' TO GRADE
                   WHEN WS-GPA > 1.7
                       MOVE 'C' TO GRADE
                   WHEN WS-GPA > 1.3
                       MOVE 'C-' TO GRADE
                   WHEN WS-GPA > 1.0
                       MOVE 'D+' TO GRADE
                   WHEN WS-GPA > 0.7
                       MOVE 'D' TO GRADE
                   WHEN WS-GPA > 0.0
                       MOVE 'D-' TO GRADE
                   WHEN OTHER
                       MOVE 'E' TO GRADE
               END-EVALUATE
               STRING GRADE DELIMITED BY SIZE INTO RESULT(I)
           END-PERFORM.

       GOBACK.
       END PROGRAM NUMERICAL-LETTER-GRADE.
