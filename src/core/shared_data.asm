; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
shared_c_MAX_PLAYERS = 4
shared_c_MAX_NAME_LEN = 10
shared_c_MAX_TEAM_NAME_LEN = (shared_c_MAX_NAME_LEN*2)+1

shared_l_PLAYER_SKIN_TONES      !byte   LIGHT_RED,BROWN   
shared_l_PLAYER_HAIR_COLORS     !byte   RED,BLACK
shared_l_PLAYER_SHIRT_COLORS    !byte   BLUE,VIOLET,RED,GREY3,WHITE,BLACK
shared_c_MALE = 0
shared_c_FEMALE = 1
shared_c_NUM_SHIRT_COLORS = 6

shared_l_NAME_OFFSETS    
    !byte   0
    !byte   shared_c_MAX_NAME_LEN
    !byte   shared_c_MAX_NAME_LEN*2
    !byte   shared_c_MAX_NAME_LEN*3
    !byte   shared_c_MAX_NAME_LEN*4

shared_l_NAME_ADDR_LO
    !byte <shared_v_player_names
    !byte <(shared_v_player_names+shared_c_MAX_NAME_LEN)
    !byte <(shared_v_player_names+(2*shared_c_MAX_NAME_LEN))
    !byte <(shared_v_player_names+(3*shared_c_MAX_NAME_LEN))
shared_l_NAME_ADDR_HI
    !byte >shared_v_player_names
    !byte >(shared_v_player_names+shared_c_MAX_NAME_LEN)
    !byte >(shared_v_player_names+(2*shared_c_MAX_NAME_LEN))
    !byte >(shared_v_player_names+(3*shared_c_MAX_NAME_LEN))

shared_l_TEAM_NAME_ADDR_LO
    !byte <shared_v_team_names 
    !byte <(shared_v_team_names+shared_c_MAX_TEAM_NAME_LEN)
shared_l_TEAM_NAME_ADDR_HI
    !byte >shared_v_team_names 
    !byte >(shared_v_team_names+shared_c_MAX_TEAM_NAME_LEN)

shared_c_STROKE_PLAY    = 0
shared_c_MATCH_PLAY     = 1

; Which ASCII character represents an overall score of 0 depends on scoring
; system.  Use 'shared_v_scoring' as index into this table.
shared_l_CHARS_FOR_SCORE_ZERO   !byte   69,SCR_CODE_EQUALS

shared_c_NUM_COURSES = 6

shared_c_PLAYING_18_HOLES   = 0
shared_c_PLAYING_FRONT_9    = 1
shared_c_PLAYING_BACK_9     = 2


; *****************
; *** VARIABLES ***
; *****************
; Player attributes.
shared_v_player_names   !fill   shared_c_MAX_PLAYERS*shared_c_MAX_NAME_LEN,SCR_CODE_SPACE
shared_v_num_players    !byte   0
; FIXME: may or may not need these...
shared_v_player_name_lens           !fill   shared_c_MAX_PLAYERS
shared_v_player_name_indices        !byte   0,1,2,3
shared_v_player_genders             !fill   shared_c_MAX_PLAYERS
shared_v_player_joysticks           !fill   shared_c_MAX_PLAYERS 
shared_v_player_skin_tones          !fill   shared_c_MAX_PLAYERS
shared_v_player_shirt_color_indices !fill   shared_c_MAX_PLAYERS

shared_v_team_names !fill   (4*shared_c_MAX_NAME_LEN)+2   
shared_v_team_names_offsets_begin   !fill   4
; Which team does each player belong to?  Pattern will be either
; 0,1 (two players) or 0,0,1,1 (four players).
shared_v_team_membership    !byte   0,0,1,1     
shared_v_team_lens          !fill   2

; The course & settings.
; NOTE: use this index to access lookup tables with information about 
; each available course?!
shared_v_course_index   !byte   0
shared_v_scoring    !byte   0
; This is either 18, front 9 or back 9.
shared_v_holes      !byte   0
; For the next 3, a value in range [0,3).
shared_v_sand_traps_difficulty  !byte   0
shared_v_greens_difficulty      !byte   0
shared_v_wind_difficulty        !byte   0
; NOTE: for your convenience!
; Works as long as the constant 'shared_c_MATCH_PLAY' is set to 1.
shared_v_is_match_play = shared_v_scoring          

shared_v_random_seed    !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

