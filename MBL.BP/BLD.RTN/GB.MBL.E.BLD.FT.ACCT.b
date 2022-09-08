* @ValidationCode : MjotMjA4OTAyOTY1MzpDcDEyNTI6MTYzMjM4OTc2NjE2MTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 Sep 2021 15:36:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.E.BLD.FT.ACCT(ENQ.DATA)
*-----------------------------------------------------------------------------
* Description: This is developed for a special purpose as the FT.ID came from the selection
*               & needed to consider ACC.NUM in the selection
*-----------------------------------------------------------------------------
*Modification 1
*  Modified by MD SHIBLI MOLLAH FDS -- on 21ST DEC 2020
*-----------------------------------------------------------------------------
* Modification 2
* Extra package removed
* Modified by MD SHIBLI MOLLAH FDS -- on 07TH JUL 2021
* CONSIDERED FOR TT AS WELL           on 23RD SEP 2021
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess

    LOCATE "ACCOUNT.NUMBER" IN ENQ.DATA<2,1> SETTING FT.POS THEN
        Y.ACC.NO = ENQ.DATA<4,FT.POS>
    END

    LOCATE "LIMIT.REF" IN ENQ.DATA<2,1> SETTING LIM.POS THEN
        Y.LIM.ID = ENQ.DATA<4,LIM.POS>
    END
 
    FN.FT = "F.FUNDS.TRANSFER$NAU"
    F.FT = ""

    FN.TT = "F.TELLER$NAU"
    F.TT = ""
******************IMAGE FOR FT*****************************
    Y.ID.LEN = Y.ACC.NO[1,2]
    IF Y.ID.LEN EQ 'FT' THEN
        Y.FT.NO = Y.ACC.NO
    
        EB.DataAccess.Opf(FN.FT, F.FT)
        EB.DataAccess.FRead(FN.FT, Y.FT.NO, REC.FT, F.FT, ERR.FT)
        Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
    END
  
*******************IMAGE FOR TT*************************************
    Y.ID.LEN = Y.ACC.NO[1,2]
    IF Y.ID.LEN EQ 'TT' THEN
        Y.TT.NO = Y.ACC.NO
    
        EB.DataAccess.Opf(FN.TT, F.TT)
        EB.DataAccess.FRead(FN.TT, Y.TT.NO, REC.TT, F.TT, ERR.TT)
        Y.ACC.NO = REC.TT<TT.Contract.Teller.TeAccountTwo>
    END

********************************************************************
    
    EB.DataAccess.Opf(FN.FT, F.FT)
      
    EB.DataAccess.FRead(FN.FT, Y.FT.NO, REC.FT, F.FT, ERR.FT)
    Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
    
    ENQ.DATA<4,FT.POS> = Y.ACC.NO
    ENQ.DATA<4,LIM.POS> = ""
    
RETURN
END