SUBROUTINE GB.MBL.E.BLD.BAL.AC.NO(ENQ.DATA)
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess
    $USING AC.AccountOpening
        
    
    FN.TT = 'F.TELLER$NAU'
    F.TT = ''
    EB.DataAccess.Opf(FN.TT, F.TT)
    
    FN.FT = 'F.FUNDS.TRANSFER$NAU'
    F.FT = ''
    EB.DataAccess.Opf(FN.FT, F.FT)

    LOCATE "ACCOUNT.NUMBER" IN ENQ.DATA<2,1> SETTING Y.DATA.POS THEN
        Y.DATA = ENQ.DATA<4,2>
    END
    
    IF Y.DATA[1,2] EQ 'TT' THEN
        EB.DataAccess.FRead(FN.TT, Y.DATA, REC.TT, F.TT, ERR.TT)
        Y.ACC.NO = REC.TT<TT.Contract.Teller.TeAccountTwo>
    END
    
    IF Y.DATA[1,2] EQ 'FT' THEN
        EB.DataAccess.FRead(FN.FT, Y.DATA, REC.FT, F.FT, ERR.FT)
        Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
    END
    
    
    ENQ.DATA<4,1> = Y.ACC.NO
    ENQ.DATA<3,Y.DATA.POS> = 'EQ'
    ENQ.DATA<4,Y.DATA.POS> = Y.ACC.NO

RETURN
END
