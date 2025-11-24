{"task_id":"HumanEval/17","prompt":"   
    IDENTIFICATION DIVISION.
	PROGRAM-ID. PARSE-MUSIC.
	
	
	ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	DATA DIVISION.
    LINKAGE SECTION.
	01 LINKED-ITEMS.
	05 L-MUSIC-STRING PIC X(100).
	05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).
	* Input to this function is a string representing musical notes in a special ASCII format.
	* Your task is to parse this string and return list of integers corresponding to how many beats does each\n      * not last.
	*   
    * Here is a legend:
	* 'o' - whole note, lasts four beats
    * 'o|' - half note, lasts two beats
	* '.|' - quater note, lasts one beat
	* 
	* >>> parse_music('o o| .| o| o| .| .| .| .| o o')
    * [4, 2, 1, 2, 2, 1, 1, 1, 1, 4, 4]
	* 
	* Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
    * Store the result in the RESULT variable and mark the end of your program with END PROGRAM
	WORKING-STORAGE SECTION.
	","entry_point":"parse_music",
	
	"canonical_solution":"   
	note_map = {'o': 4, 'o|': 2, '.|': 1}
	return [note_map[x] for x in music_string.split(' ') if x],
	
	"tests":[{"test":"  
	IDENTIFICATION DIVISION.
	PROGRAM-ID. PARSE-MUSIC-CALL.
    ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
	SELECT OUTPUT-FILE ASSIGN TO \"PARSE-MUSIC.TXT\"
	ORGANIZATION IS LINE SEQUENTIAL
	STATUS IS OUTPUT-FILE-STATUS.
	DATA DIVISION.
	FILE SECTION.
	FD OUTPUT-FILE.
	01 OUTPUT-RECORD PIC S9(10) SIGN LEADING.
	WORKING-STORAGE SECTION.
	01 OUTPUT-FILE-STATUS PIC X(02).
	01 LINKED-ITEMS.
	05 L-MUSIC-STRING PIC X(100).
	05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).
	PROCEDURE DIVISION.
	MOVE '' TO L-MUSIC-STRING
	CALL \"PARSE-MUSIC\" USING LINKED-ITEMS


	OPEN OUTPUT OUTPUT-FILE

	IF OUTPUT-FILE-STATUS NOT = \"00\"
	DISPLAY \"ERROR OPENING OUTPUT FILE\"
	STOP RUN
	END-IF
	PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 100
	MOVE RESULT (NI) TO OUTPUT-RECORD
	WRITE OUTPUT-RECORD
	END-PERFORM
	IF OUTPUT-FILE-STATUS NOT = \"00\"
	DISPLAY \"ERROR WRITING TO OUTPUT FILE\
	STOP RUN\n        END-IF\n\n        CLOSE OUTPUT-FILE.

	","result":{"value":"[]","type_":{"List":"Int"}}},{"test":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PARSE-MUSIC-CALL.\n\n       ENVIRONMENT DIVISION.\n\n       INPUT-OUTPUT SECTION.\n\n       FILE-CONTROL.\n\n       SELECT OUTPUT-FILE ASSIGN TO \"PARSE-MUSIC.TXT\"\n           ORGANIZATION IS LINE SEQUENTIAL\n           STATUS IS OUTPUT-FILE-STATUS.\n       \n       DATA DIVISION.\n\n       FILE SECTION.\n       FD OUTPUT-FILE.\n       01 OUTPUT-RECORD PIC S9(10) SIGN LEADING.\n\n       WORKING-STORAGE SECTION.\n\n       01 OUTPUT-FILE-STATUS PIC X(02).\n\n\n\n       01 LINKED-ITEMS.\n           05 L-MUSIC-STRING PIC X(100).\n           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).\n\n       PROCEDURE DIVISION.\n\n       MOVE 'o o o o' TO L-MUSIC-STRING\n\n       CALL \"PARSE-MUSIC\" USING LINKED-ITEMS\n       \n       OPEN OUTPUT OUTPUT-FILE\n\n       IF OUTPUT-FILE-STATUS NOT = \"00\"\n           DISPLAY \"ERROR OPENING OUTPUT FILE\"\n           STOP RUN\n        END-IF\n\n       PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 100\n           MOVE RESULT (NI) TO OUTPUT-RECORD\n           WRITE OUTPUT-RECORD\n       END-PERFORM\n\n        IF OUTPUT-FILE-STATUS NOT = \"00\"\n            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"\n            STOP RUN\n        END-IF\n\n        CLOSE OUTPUT-FILE\n        .\n       ","result":{"value":"[4, 4, 4, 4]","type_":{"List":"Int"}}},{"test":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PARSE-MUSIC-CALL.\n\n       ENVIRONMENT DIVISION.\n\n       INPUT-OUTPUT SECTION.\n\n       FILE-CONTROL.\n\n       SELECT OUTPUT-FILE ASSIGN TO \"PARSE-MUSIC.TXT\"\n           ORGANIZATION IS LINE SEQUENTIAL\n           STATUS IS OUTPUT-FILE-STATUS.\n       \n       DATA DIVISION.\n\n       FILE SECTION.\n       FD OUTPUT-FILE.\n       01 OUTPUT-RECORD PIC S9(10) SIGN LEADING.\n\n       WORKING-STORAGE SECTION.\n\n       01 OUTPUT-FILE-STATUS PIC X(02).\n\n\n\n       01 LINKED-ITEMS.\n           05 L-MUSIC-STRING PIC X(100).\n           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).\n\n       PROCEDURE DIVISION.\n\n       MOVE '.| .| .| .|' TO L-MUSIC-STRING\n\n       CALL \"PARSE-MUSIC\" USING LINKED-ITEMS\n       \n       OPEN OUTPUT OUTPUT-FILE\n\n       IF OUTPUT-FILE-STATUS NOT = \"00\"\n           DISPLAY \"ERROR OPENING OUTPUT FILE\"\n           STOP RUN\n        END-IF\n\n       PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 100\n           MOVE RESULT (NI) TO OUTPUT-RECORD\n           WRITE OUTPUT-RECORD\n       END-PERFORM\n\n        IF OUTPUT-FILE-STATUS NOT = \"00\"\n            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"\n            STOP RUN\n        END-IF\n\n        CLOSE OUTPUT-FILE\n        .\n       ","result":{"value":"[1, 1, 1, 1]","type_":{"List":"Int"}}},{"test":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PARSE-MUSIC-CALL.\n\n       ENVIRONMENT DIVISION.\n\n       INPUT-OUTPUT SECTION.\n\n       FILE-CONTROL.\n\n       SELECT OUTPUT-FILE ASSIGN TO \"PARSE-MUSIC.TXT\"\n           ORGANIZATION IS LINE SEQUENTIAL\n           STATUS IS OUTPUT-FILE-STATUS.\n       \n       DATA DIVISION.\n\n       FILE SECTION.\n       FD OUTPUT-FILE.\n       01 OUTPUT-RECORD PIC S9(10) SIGN LEADING.\n\n       WORKING-STORAGE SECTION.\n\n       01 OUTPUT-FILE-STATUS PIC X(02).\n\n\n\n       01 LINKED-ITEMS.\n           05 L-MUSIC-STRING PIC X(100).\n           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).\n\n       PROCEDURE DIVISION.\n\n       MOVE 'o| o| .| .| o o o o' TO L-MUSIC-STRING\n\n       CALL \"PARSE-MUSIC\" USING LINKED-ITEMS\n       \n       OPEN OUTPUT OUTPUT-FILE\n\n       IF OUTPUT-FILE-STATUS NOT = \"00\"\n           DISPLAY \"ERROR OPENING OUTPUT FILE\"\n           STOP RUN\n        END-IF\n\n       PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 100\n           MOVE RESULT (NI) TO OUTPUT-RECORD\n           WRITE OUTPUT-RECORD\n       END-PERFORM\n\n        IF OUTPUT-FILE-STATUS NOT = \"00\"\n            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"\n            STOP RUN\n        END-IF\n\n        CLOSE OUTPUT-FILE\n        .\n       ","result":{"value":"[2, 2, 1, 1, 4, 4, 4, 4]","type_":{"List":"Int"}}},{"test":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PARSE-MUSIC-CALL.\n\n       ENVIRONMENT DIVISION.\n\n       INPUT-OUTPUT SECTION.\n\n       FILE-CONTROL.\n\n       SELECT OUTPUT-FILE ASSIGN TO \"PARSE-MUSIC.TXT\"\n           ORGANIZATION IS LINE SEQUENTIAL\n           STATUS IS OUTPUT-FILE-STATUS.\n       \n       DATA DIVISION.\n\n       FILE SECTION.\n       FD OUTPUT-FILE.\n       01 OUTPUT-RECORD PIC S9(10) SIGN LEADING.\n\n       WORKING-STORAGE SECTION.\n\n       01 OUTPUT-FILE-STATUS PIC X(02).\n\n\n\n       01 LINKED-ITEMS.\n           05 L-MUSIC-STRING PIC X(100).\n           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).\n\n       PROCEDURE DIVISION.\n\n       MOVE 'o| .| o| .| o o| o o|' TO L-MUSIC-STRING\n\n       CALL \"PARSE-MUSIC\" USING LINKED-ITEMS\n       \n       OPEN OUTPUT OUTPUT-FILE\n\n       IF OUTPUT-FILE-STATUS NOT = \"00\"\n           DISPLAY \"ERROR OPENING OUTPUT FILE\"\n           STOP RUN\n        END-IF\n\n       PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 100\n           MOVE RESULT (NI) TO OUTPUT-RECORD\n           WRITE OUTPUT-RECORD\n       END-PERFORM\n\n        IF OUTPUT-FILE-STATUS NOT = \"00\"\n            DISPLAY \"ERROR WRITING TO OUTPUT FILE\"\n            STOP RUN\n        END-IF\n\n        CLOSE OUTPUT-FILE\n        .\n       ","result":{"value":"[2, 1, 2, 1, 4, 2, 4, 2]","type_":{"List":"Int"}}}]}
