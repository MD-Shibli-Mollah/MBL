* @ValidationCode : MjotMTE1OTEzMjc1OkNwMTI1MjoxNjMxNjE0OTIxOTc4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 14 Sep 2021 16:22:01
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
*
*
*  Modified by MD SHIBLI MOLLAH FDS -- on 7TH SEP 2021
*  ALTERNATE ACC MANAGED
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING AC.AccountOpening
    $USING EB.Reports
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING AA.Framework
     
    LOCATE "ARRANGEMENT" IN ENQ.DATA<2,1> SETTING ARR.POS THEN
        ARR.ID = ENQ.DATA<4,ARR.POS>
    END
    
    IF Y.ARR.ID EQ 'AA' THEN
        ENQ.DATA<4,ARR.POS> = ARR.ID
        RETURN
    END ELSE
    
        FN.ACC = "F.ACCOUNT"
        F.ACC = ""
   
        FN.ALT.AC = 'F.ALTERNATE.ACCOUNT'
        F.ALT.AC = ''
    
        EB.DataAccess.Opf(FN.ALT.AC, F.ALT.AC)
        EB.DataAccess.Opf(FN.ACC, F.ACC)
    
        EB.DataAccess.FRead(FN.ALT.AC, ARR.ID, R.ALT.AC, F.ALT.AC, ERR.ALT.AC)
        Y.R19.AC.ID = R.ALT.AC<AC.AccountOpening.AlternateAccount.AacGlobusAcctNumber>
    
        EB.DataAccess.FRead(FN.ACC, Y.R19.AC.ID, REC.ACCT, F.ACC, ERR)
        Y.ARR.ID = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
        ENQ.DATA<4,ARR.POS> = Y.ARR.ID
        
    END
  
RETURN

END
