    SUBROUTINE MBL.BUILD.ENQ.FT.HIST(ENQ.DATA)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
!    DEBUG
    LOCATE 'DEBIT.VALUE.DATE' IN ENQ.DATA<2,1> SETTING D.DATE.POS THEN
        Y.DATE=ENQ.DATA<4,D.DATE.POS>
        CALL JULDATE(Y.DATE, Y.JULIAN.DAY)
        Y.JULIAN.DAY=Y.JULIAN.DAY[3,5]
        Y.FT.ID='FT':Y.JULIAN.DAY
    END
    LOCATE '@ID' IN ENQ.DATA<2,1> SETTING Y.ID.POS ELSE NULL
    ENQ.DATA<2,Y.ID.POS>='@ID'
    ENQ.DATA<3,Y.ID.POS>='LK'
    ENQ.DATA<4,Y.ID.POS>=Y.FT.ID:'...'
END
