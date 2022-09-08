* @ValidationCode : MjotMTQzNzQzNzMzMDpDcDEyNTI6MTYyNjUyODM4MTcxNTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Jul 2021 19:26:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.V.TT.BR
    
***********************************************************************
*
* Company Name   : FDS BD LTD
* Developed By   : MD SHIBLI MOLLAH
* Modified By    :
*----------------------------------------------------------------------
*Subroutine Type:
*
*Attached To    : Teller Versions(TELLER,MBL.LCY.CASHCHQ, TELLER,MBL.LCY.CASHIN
*                 TELLER,MBL.LCY.CASHWDL, TELLER,MBL.SDSA.LCY.CASHIN, TELLER,MBL.SDSA.LCY.CASHIN.SUSP
*                 TELLER,MBL.SDSA.LCY.CASHWDL, TELLER,MBL.SDSA.LCY.CASHWDL.SUSP
*
*Attached As    : VALIDATION ROUTINE FOR THE FIELD "ACCOUNT.2"
*
*------------------------------------------
*Description
*------------
* This Routine will Fetch the BRANCH ID OF ACCOUNT.2 BRANCH INFORMATION AND SET IT
* to the Local Field LT.BRANCH
*------------------------------------------------------------------------------------
* Modification History : Unnecessary INSERTS ARE COMMENTED OUT
*
* Modified by MD SHIBLI MOLLAH FDS -- on 17TH JUL 2021
*
*-----------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_GTS.COMMON
*    $INSERT I_EB.TRANS.COMMON
*    $INSERT I_ENQUIRY.COMMON
    $USING EB.SystemTables
* $USING EB.Updates
    $USING TT.Contract
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING AC.AccountOpening
    
    
    Y.ACC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
     
*   IF OFS$OPERATION EQ 'BUILD' THEN
        
    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    Y.TT.LT.BRANCH = ""
    
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    
    EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
    Y.CO.CODE = R.ACC<AC.AccountOpening.Account.CoCode>

    APPLICATION.NAME = 'TELLER'
    Y.FILED.NAME = 'LT.BRANCH'
    Y.FIELD.POS = ''
    EB.LocalReferences.GetLocRef(APPLICATION.NAME,Y.FILED.NAME,Y.FIELD.POS)
    Y.TT.LT.BRANCH = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
    Y.TT.LT.BRANCH<1,Y.FIELD.POS> = Y.CO.CODE
    EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef,Y.TT.LT.BRANCH)

RETURN
*    END
END
