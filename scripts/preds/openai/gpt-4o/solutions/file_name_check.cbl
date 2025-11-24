       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-NAME-CHECK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  WS-FILE-NAME                PIC X(100).
       01  WS-BEFORE-DOT               PIC X(97).
       01  WS-AFTER-DOT                PIC X(3).
       01  WS-DOT-POS                  PIC 9(3).
       01  WS-DIGIT-COUNT              PIC 9(3) VALUE 0.
       01  WS-I                        PIC 9(3). 
       01  WS-VALID-EXTENSIONS         PIC X(9) VALUE "txtexedll".

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-FILE-NAME PIC X(100).
           05 RESULT PIC X(100).

      * Create a function which takes a string representing a file's name, and returns
      * 'Yes' if the the file's name is valid, and returns 'No' otherwise.
      * A file's name is considered to be valid if and only if all the following conditions
      * are met:
      * - There should not be more than three digits ('0'-'9') in the file's name.
      * - The file's name contains exactly one dot '.'
      * - The substring before the dot should not be empty, and it starts with a letter from
      * the latin alphapet ('a'-'z' and 'A'-'Z').
      * - The substring after the dot should be one of these: ['txt', 'exe', 'dll']
      * Examples:
      * file_name_check("example.txt") # => 'Yes'
      * file_name_check("1example.dll") # => 'No' (the name should start with a latin alphapet letter)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE L-FILE-NAME TO WS-FILE-NAME
           
           INSPECT WS-FILE-NAME
             TALLYING WS-DOT-POS FOR ALL "."
             
           IF WS-DOT-POS NOT = 1
              MOVE "No" TO RESULT
           ELSE
              PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
                 EVALUATE TRUE
                      WHEN WS-I > FUNCTION LENGTH(WS-FILE-NAME)
                          EXIT PERFORM
                      WHEN WS-FILE-NAME(WS-I:1) = "." 
                          MOVE WS-I TO WS-DOT-POS 
                          EXIT PERFORM
                      WHEN FUNCTION DIGIT(WS-FILE-NAME(WS-I:1))
                          ADD 1 TO WS-DIGIT-COUNT 
                 END-EVALUATE
              END-PERFORM

              IF WS-DIGIT-COUNT > 3 
                 MOVE "No" TO RESULT
              ELSE
                 MOVE WS-FILE-NAME(1:WS-DOT-POS - 1) TO WS-BEFORE-DOT
                 MOVE WS-FILE-NAME(WS-DOT-POS + 1:3) TO WS-AFTER-DOT
                 
                 IF WS-BEFORE-DOT(1:1) NOT >= "A" AND WS-BEFORE-DOT(1:1) NOT <= "Z" AND
                    WS-BEFORE-DOT(1:1) NOT >= "a" AND WS-BEFORE-DOT(1:1) NOT <= "z"
                    MOVE "No" TO RESULT
                 ELSE IF FUNCTION LENGTH(WS-AFTER-DOT) NOT = 3 OR
                          FUNCTION COLLATE(WS-AFTER-DOT, WS-VALID-EXTENSIONS) = 0
                    MOVE "No" TO RESULT
                 ELSE
                    MOVE "Yes" TO RESULT
              END-IF
           END-IF.

           GOBACK.

       END PROGRAM FILE-NAME-CHECK.
