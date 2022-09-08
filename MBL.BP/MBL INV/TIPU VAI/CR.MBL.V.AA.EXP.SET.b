* @ValidationCode : MjotMTg1NjM3Mzk0NzpDcDEyNTI6MTYyODUyOTAwMDA3NTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Aug 2021 23:10:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.V.AA.EXP.SET

*-----------------------------------------------------------------------------
* Modification History :
*Subroutine Description: Validation routine for Expired Loan(AA ID or Acc Num)
*Subroutine Type:
*Attached To    :  ACTIVITY.API - MBL.ACCT.TITLE.API, Property Class: SETTLEMENT, Action: UPDATE
*Attached As    : RECORD ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 09/08/2021 -                            Developed By   - MD. Shibli Mollah
*                                                          FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    
    $USING AC.AccountOpening
    $USING AA.Settlement
    $USING AA.Framework
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.DataAccess
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    
    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''

    FLD.POS = ''
RETURN
*-----------------------------------------------------------------------------

OPENFILES:

    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.AA.ARR, F.AA.ARR)

RETURN
*-----------------------------------------------------------------------------

PROCESS:
    
*--------Map Local fields----------------------------------------------------------------
    APPLICATION.NAME = 'AA.ARRANGEMENT.ACTIVITY'
    LOCAL.FIELDS = 'LT.EXP.LN.ID'
    EB.Updates.MultiGetLocRef(APPLICATION.NAME, LOCAL.FIELDS, FLD.POS)
    Y.EXP.LN.ID.POS = FLD.POS<1,1>
*--------END----------------------------------------------------------------

    Y.LOC.DATA = c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActLocalRef>
    Y.AA.ID = Y.LOC.DATA<1, Y.EXP.LN.ID.POS>
    
    Y.AA.LEN = Y.AA.ID[1,2]
*Y.AA.ID = c_aalocArrId

*-----------------------AA ID CHECK-----------------------------------------
    IF Y.AA.LEN NE 'AA' THEN
        Y.ACC = Y.AA.ID
    
        EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
* Y.CUS.ID = R.ACC<AC.AccountOpening.Account.Customer>
        Y.AA.ID = R.ACC<AC.AccountOpening.Account.ArrangementId>
    END
    
    IF Y.AA.LEN EQ 'AA' THEN
        EB.DataAccess.FRead(FN.AA.ARR, Y.AA.ID, R.ARR, F.AA.ARR, ARR.ERR)
        Y.ACC = R.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
    END
*---------------------------END-------------------------------------------

*-------------------------------AA.ARR.LIMIT-----------------------------------------------
    PROP.CLASS.SET = 'SETTLEMENT'
    AA.Framework.GetArrangementConditions(Y.AA.ID, PROP.CLASS.SET, PROPERTY, '', RETURN.IDS.SET, RETURN.VALUES.SET, ERR.MSG.SET)
    R.SETTLEMENT = RAISE(RETURN.VALUES.SET)
* PAYOUT.ACTIVITY: LENDING-APPLYPAYMENT-PR.PAYOFF
    Y.PAYOUT.ACTIVITY = "LENDING-APPLYPAYMENT-PR.PAYOFF"
    Y.PAYOUT.ACC = Y.ACC

**SET LIMIT DATA
    EB.SystemTables.setRNew(AA.Settlement.Settlement.SetPayoutActivity, Y.PAYOUT.ACTIVITY)
    EB.SystemTables.setRNew(AA.Settlement.Settlement.SetPayoutAccount, Y.PAYOUT.ACC)
    
*-------------------------------TRACER-------------------------------------------------
*    WriteData = Y.AA.ID:" R.SETTLEMENT: ":R.SETTLEMENT:" Y.PAYOUT.ACTIVITY: ":Y.PAYOUT.ACTIVITY
*    FileName = 'SHIBLI_CR.AA.SETTLEMENT.txt'
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