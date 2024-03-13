*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWFT_INV_TYPE...................................*
DATA:  BEGIN OF STATUS_ZWFT_INV_TYPE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFT_INV_TYPE                 .
CONTROLS: TCTRL_ZWFT_INV_TYPE
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZWFT_MENU.......................................*
DATA:  BEGIN OF STATUS_ZWFT_MENU                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFT_MENU                     .
CONTROLS: TCTRL_ZWFT_MENU
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZWFT_SCREEN.....................................*
DATA:  BEGIN OF STATUS_ZWFT_SCREEN                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFT_SCREEN                   .
CONTROLS: TCTRL_ZWFT_SCREEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWFT_INV_TYPE                 .
TABLES: *ZWFT_MENU                     .
TABLES: *ZWFT_SCREEN                   .
TABLES: ZWFT_INV_TYPE                  .
TABLES: ZWFT_MENU                      .
TABLES: ZWFT_SCREEN                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
