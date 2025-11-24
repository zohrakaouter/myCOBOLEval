       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-NAME-CHECK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-DOT-COUNT PIC 9(2) VALUE 0.
           05 WS-DIGIT-COUNT PIC 9(2) VALUE 0.
           05 WS-DOT-POS PIC 9(3) VALUE 0.
           05 WS-LEN PIC 9(3) VALUE 0.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-VALID-EXT PIC X(3) VALUE SPACES.
           05 WS-FIRST-CHAR PIC X VALUE SPACE.
           05 WS-VALID-FLAG PIC X VALUE 'Y'.
               88 VALID VALUE 'Y'.
               88 INVALID VALUE 'N'.

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
           MOVE SPACES TO RESULT

           * Check if the file name is empty
           IF L-FILE-NAME = SPACES
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           * Get the length of the file name
           INSPECT FUNCTION REVERSE(L-FILE-NAME) TALLYING WS-LEN
               FOR CHARACTERS BEFORE INITIAL SPACE

           * Check for more than three digits
           INSPECT L-FILE-NAME(1:WS-LEN) TALLYING WS-DIGIT-COUNT
               FOR ALL '0123456789'
           IF WS-DIGIT-COUNT > 3
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           * Check for exactly one dot
           INSPECT L-FILE-NAME(1:WS-LEN) TALLYING WS-DOT-COUNT
               FOR ALL '.'
           IF WS-DOT-COUNT NOT = 1
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           * Find the position of the dot
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               IF L-FILE-NAME(WS-I:1) = '.'
                   MOVE WS-I TO WS-DOT-POS
                   EXIT PERFORM
               END-IF
           END-PERFORM

           * Check if the substring before the dot is not empty
           IF WS-DOT-POS = 1
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           * Check if the first character is a latin alphabet letter
           MOVE L-FILE-NAME(1:1) TO WS-FIRST-CHAR
           IF NOT (WS-FIRST-CHAR >= 'A' AND WS-FIRST-CHAR <= 'Z' OR
                   WS-FIRST-CHAR >= 'a' AND WS-FIRST-CHAR <= 'z')
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           * Check the substring after the dot
           IF WS-DOT-POS + 3 > WS-LEN
               MOVE 'No' TO RESULT
               GOBACK
           END-IF

           MOVE L-FILE-NAME(WS-DOT-POS + 1:3) TO WS-VALID-EXT
           IF WS-VALID-EXT = 'txt' OR WS-VALID-EXT = 'exe' OR
              WS-VALID-EXT = 'dll'
               MOVE 'Yes' TO RESULT
           ELSE
               MOVE 'No' TO RESULT
           END-IF

           GOBACK.
       END PROGRAM FILE-NAME-CHECK.
