* @ValidationCode : MjotNzA0NDY1NTA5OkNwMTI1MjoxNjI1NjY1Njc0MDA3OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 07 Jul 2021 19:47:54
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
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING FT.Contract
    $USING EB.DataAccess

    LOCATE "ACCOUNT.NUMBER" IN ENQ.DATA<2,1> SETTING FT.POS THEN
        Y.FT.ID = ENQ.DATA<4,FT.POS>
    END

    LOCATE "LIMIT.REF" IN ENQ.DATA<2,1> SETTING LIM.POS THEN
        Y.LIM.ID = ENQ.DATA<4,LIM.POS>
    END

*----------IF FT.ID IS PASSED BY USR-------------------------
*  Y.FT.ID.1 = FIELD(FT.ID," ",2,1)
*-------------------------------------------------------------
    Y.FT.NO = Y.FT.ID
    
    FN.FT = "F.FUNDS.TRANSFER$NAU"
    F.FT = ""
   
    EB.DataAccess.FRead(FN.FT, Y.FT.NO, REC.FT, F.FT, ERR.FT)
    Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
    
    ENQ.DATA<4,FT.POS> = Y.ACC.NO
    ENQ.DATA<4,LIM.POS> = ""
    
RETURN
END