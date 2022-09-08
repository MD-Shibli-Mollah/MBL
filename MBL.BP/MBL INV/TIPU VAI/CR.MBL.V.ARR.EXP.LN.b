* @ValidationCode : MjoxNDE5OTczNDU1OkNwMTI1MjoxNjI4MDA4ODg1MzQ2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 Aug 2021 22:41:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.V.ARR.EXP.LN
*-----------------------------------------------------------------------------
*Subroutine Description: Validation routine for Expired Loan(AA or Acc Num)
*Subroutine Type:
*Attached To    :  Version - AA.ARRANGEMENT.ACTIVITY,MBL.AA.EXP.NEW
*Attached As    :  VALIDATION ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 17/07/2021 -                            Developed By   - MD. Shibli Mollah
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    $USING EB.SystemTables
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING AC.AccountOpening
    $USING AA.Framework
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.AA.ARR, F.AA.ARR)
    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN

PROCESS:

    Y.ID = EB.SystemTables.getComi()
    Y.ID.LEN = Y.ID[1,2]
    
    IF Y.ID.LEN EQ 'AA' THEN
        EB.DataAccess.FRead(FN.AA.ARR, Y.ID, R.ARR, F.AA.ARR, ARR.ERR)
        Y.CUS.ID = R.ARR<AA.Framework.Arrangement.ArrCustomer>
        Y.PRODUCT = R.ARR<AA.Framework.Arrangement.ArrProduct>
        Y.CUS.ROLE = R.ARR<AA.Framework.Arrangement.ArrCustomerRole>
        Y.PRODUCT.LINE = R.ARR<AA.Framework.Arrangement.ArrProductLine>
        
        IF Y.PRODUCT.LINE EQ "ACCOUNTS" THEN
            Y.ACTIVITY = "ACCOUNTS-MBL.EXP.RESCHEDULE-ARRANGEMENT"
        END
        
        IF Y.PRODUCT.LINE EQ "LENDING" THEN
            Y.ACTIVITY = "LENDING-MBL.EXP.RESCHEDULE-ARRANGEMENT"
        END
    END
    
    IF Y.ID.LEN NE 'AA' THEN
        Y.ACC = Y.ID
    
        EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
        Y.CUS.ID = R.ACC<AC.AccountOpening.Account.Customer>
        Y.AA.ID = R.ACC<AC.AccountOpening.Account.ArrangementId>
        
        EB.DataAccess.FRead(FN.AA.ARR, Y.AA.ID, R.ARR, F.AA.ARR, ARR.ERR)
        Y.PRODUCT = R.ARR<AA.Framework.Arrangement.ArrProduct>
        Y.CUS.ROLE = R.ARR<AA.Framework.Arrangement.ArrCustomerRole>
        Y.PRODUCT.LINE = R.ARR<AA.Framework.Arrangement.ArrProductLine>
        
        IF Y.PRODUCT.LINE EQ "ACCOUNTS" THEN
            Y.ACTIVITY = "ACCOUNTS-MBL.EXP.RESCHEDULE-ARRANGEMENT"
        END
        
        IF Y.PRODUCT.LINE EQ "LENDING" THEN
            Y.ACTIVITY = "LENDING-MBL.EXP.RESCHEDULE-ARRANGEMENT"
        END
    
    END
*----------------------SET VALUES------------------------------------------*
    EB.SystemTables.setRNew(AA.Framework.ArrangementActivity.ArrActCustomer, Y.CUS.ID)
    EB.SystemTables.setRNew(AA.Framework.ArrangementActivity.ArrActProduct, Y.PRODUCT)
    EB.SystemTables.setRNew(AA.Framework.ArrangementActivity.ArrActCustomerRole, Y.CUS.ROLE)
    EB.SystemTables.setRNew(AA.Framework.ArrangementActivity.ArrActActivity, Y.ACTIVITY)
    

*-------------------------------TRACER-------------------------------------------------
*    WriteData = Y.ID:" ":Y.CUS.ID:" ":Y.PRODUCT:" ":Y.ACTIVITY
*    FileName = 'SHIBLI_CR.EXP.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput

*-------------------------------TRACER END-------------------------------------------------

RETURN
END
