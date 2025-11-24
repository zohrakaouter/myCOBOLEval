{
"task_id":"HumanEval/141",
"prompt":"       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-NAME-CHECK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

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
      * file_name_check(\"example.txt\") # => 'Yes'
      * file_name_check(\"1example.dll\") # => 'No' (the name should start with a latin alphapet letter)
      
      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       WORKING-STORAGE SECTION.
       ",
"entry_point":"file_name_check",
"canonical_solution":"    suf = ['txt', 'exe', 'dll']
    lst = file_name.split(sep='.')
    if len(lst) != 2:
        return 'No'
    if not lst[1] in suf:
        return 'No'
    if len(lst[0]) == 0:
        return 'No'
    if not lst[0][0].isalpha():
        return 'No'
    t = len([x for x in lst[0] if x.isdigit()])
    if t > 3:
        return 'No'
    return 'Yes'
",
"tests":[
{
"test":"       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-NAME-CHECK-CALL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT OUTPUT-FILE ASSIGN TO \"FILE-NAME-CHECK.TXT\"
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS OUTPUT-FILE-STATUS.
       
       DATA DIVISION.

       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.

       01 OUTPUT-FILE-STATUS PIC X(02).


       01 LINKED-ITEMS.
           05 L-FILE-NAME PIC X(100).
           05 RESULT PIC X(100).

       PROCEDURE DIVISION.

       MOVE \"example.txt\" TO L-FILE-NAME

       CALL \"FILE-NAME-CHECK\" USING LINKED-ITEMS
       
       OPEN OUTPUT OUTPUT-FILE

       IF OUTPUT-FILE-STATUS NOT = \"00\"
           DISPLAY \"ERROR OPENING OUTPUT FILE\"
           STOP RUN
        END-IF

       MOVE RESULT TO OUTPUT-RECORD
       WRITE OUTPUT-RECORD


        IF OUTPUT-FILE-STATUS NOT = \"00\"
            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"
            STOP RUN
        END-IF

        CLOSE OUTPUT-FILE
        .
       ",
"result":{"value":"'Yes'","type_":"String"}
},
{
"test":"       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-NAME-CHECK-CALL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT OUTPUT-FILE ASSIGN TO \"FILE-NAME-CHECK.TXT\"
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS OUTPUT-FILE-STATUS.
       
       DATA DIVISION.

       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.

       01 OUTPUT-FILE-STATUS PIC X(02).


       01 LINKED-ITEMS.
           05 L-FILE-NAME PIC X(100).
           05 RESULT PIC X(100).

       PROCEDURE DIVISION.

       MOVE \"1example.dll\" TO L-FILE-NAME

       CALL \"FILE-NAME-CHECK\" USING LINKED-ITEMS
       
       OPEN OUTPUT OUTPUT-FILE

       IF OUTPUT-FILE-STATUS NOT = \"00\"
           DISPLAY \"ERROR OPENING OUTPUT FILE\"
           STOP RUN
        END-IF

       MOVE RESULT TO OUTPUT-RECORD
       WRITE OUTPUT-RECORD


        IF OUTPUT-FILE-STATUS NOT = \"00\"
            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"
            STOP RUN
        END-IF

        CLOSE OUTPUT-FILE
        .
       ",
"result":{"value":"'No'","type_":"String"}
}
]}
