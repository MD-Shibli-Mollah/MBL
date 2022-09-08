* @ValidationCode : MjoxNzAzMjUxNzQ5OkNwMTI1MjoxNjI1NjU5NTQ5MDkwOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 07 Jul 2021 18:05:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CM.MBL.ENQ.BLD.BR.RET.ACC.ACT(ENQ.DATA)
* Modification History : BUILD ROUTINE ACC.NO TO ARR.ID
*-----------------------------------------------------------------------------
*  Modified by MD SHIBLI MOLLAH FDS -- on 8TH DEC 2020
*  1. After getting Arrangement ID, there's no need to execute ACCOUNT Block.
* RETURN in line number 40
* Modified by MD SHIBLI MOLLAH FDS -- on 07TH JUL 2021
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING AC.AccountOpening
    $USING EB.Reports
    $USING EB.DataAccess

*    Y.FIELDS = ENQ.DATA<2>
*    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
     
    LOCATE "ARRANGEMENT" IN ENQ.DATA<2,1> SETTING ARR.POS THEN
        ARR.ID = ENQ.DATA<4,ARR.POS>
    END

*----------IF ARR.ID IS PASSED BY USR-------------------------
    Y.ARR.ID.AA = ARR.ID[1,2]
    
    IF Y.ARR.ID.AA EQ 'AA' THEN
        ENQ.DATA<4,ARR.POS> = ARR.ID
        RETURN
    END
*-------------------------------------------------------------
    Y.ACC.NO = ARR.ID
    FN.ACC="F.ACCOUNT"
    F.ACC=""
   
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, REC.ACCT, F.ACC, ERR)
    Y.ARR.ID = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
    
* EB.Reports.s
    IF Y.ARR.ID NE '' THEN
        ENQ.DATA<4,ARR.POS> = Y.ARR.ID
    END
  
RETURN
END