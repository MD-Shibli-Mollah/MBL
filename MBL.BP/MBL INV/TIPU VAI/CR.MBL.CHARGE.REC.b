* @ValidationCode : MjotOTEyOTU0OTkwOkNwMTI1MjoxNjMxMTg5MDU3Njc2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 09 Sep 2021 18:04:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.CHARGE.REC
*-----------------------------------------------------------------------------
* Modification History :
*Subroutine Description: Validation routine for Expired Loan(AA or Acc Num)
*Subroutine Type:
*Attached To    :  ACTIVITY.API - MBL.ACCT.TITLE.API, Property Class: ACCOUNT, Action: MAINTAIN
*Attached As    : RECORD ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 09/09/2021 -                            Developed By   - MD. Shibli Mollah
*                                                       FDS Services Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    
    $USING AA.Account
    $USING AC.AccountOpening
    $USING EB.SystemTables
    $USING AA.Framework
    $USING EB.Updates
    $USING EB.DataAccess
    $USING RE.ConBalanceUpdates
    
*-------------------------------TRACER-------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    FLD.POS = ''
RETURN
*-----------------------------------------------------------------------------

OPENFILES:

    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN
*-----------------------------------------------------------------------------

PROCESS:

    Y.AA.ID = c_aalocArrId

* AA.Framework.GetArrangementAccountId(Y.AA.ID, accountId, Currency, ReturnError)   ;*To get Arrangement Account
    accountId = AA.Framework.getC_aaloclinkedaccount()
* accountId = c_aalocArrActivityRec<AA.Framework.Arrangement.ArrLinkedApplId>
    Y.ACC = accountId

    EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
    Y.AC.NAME = R.ACC<AC.AccountOpening.Account.AccountTitleOne>
*  Y.W.BAL = R.ACC<AC.AccountOpening.Account.WorkingBalance>
    
    AcctId = Y.ACC
    BalanceType = "CURACCOUNT"
    RE.ConBalanceUpdates.AcGetEcbBalance(AcctId, BalanceType, SubType, BalDate, EcbBalance, EcbBalLcy)
    Y.W.BAL = EcbBalance
    
*-------------------------------TRACER-------------------------------------------------
    WriteData = "Y.AA.ID: " ::Y.AA.ID:" accountId :":accountId:" Y.W.BAL :":Y.W.BAL:" Y.AC.NAME: ":Y.AC.NAME :" BalanceType: ":BalanceType:" BalDate: ":BalDate :" EcbBalLcy: ":EcbBalLcy
    FileName = 'SHIBLI_AA.CHARGE.TRN.21.V2.txt'
    FilePath = 'DL.BP'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput

*-------------------------------TRACER END-------------------------------------------------
RETURN
********************************************************
END